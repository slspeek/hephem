{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | http://star-www.st-and.ac.uk/~fv/webnotes/chapter7.htm
module HEphem.HEphem where

import           Control.Arrow
import           Control.Lens          hiding (element)
import           Data.Angle
import           Data.Fixed            (mod')
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Vector.Class
import           GHC.Exts
import           HEphem.BSParser
import           HEphem.Data
import           HEphem.NGCParser
import           HEphem.ParserUtil
import           Text.Printf

geoAms :: GeoLoc
geoAms = GeoLoc (fromDMS 52 21 0) (fromDMS 4 51 59)

fracDays :: UTCTime -> Double
fracDays u = (fromIntegral . toModifiedJulianDay) (utctDay u) + (fromRational
                                                                   (toRational (utctDayTime u)) / 86400)

ngcSkyObjects :: [SkyObject]
ngcSkyObjects = fmap NGC ngcObjectList

brightStarObjects :: [SkyObject]
brightStarObjects = fmap Star brightstarlist

allSkyObjects :: [SkyObject]
allSkyObjects = ngcSkyObjects `mappend` brightStarObjects

brightSkyObjects :: Float -> [SkyObject]
brightSkyObjects = brightFilter allSkyObjects

brightNGCObjects :: Float -> [SkyObject]
brightNGCObjects = brightFilter ngcSkyObjects

brightFilter :: [SkyObject] -> Float -> [SkyObject]
brightFilter ss m = filter (\s -> magnitude s < m) ss

siderealtime :: UTCTime -> Deg
siderealtime ut = Degrees $ 15 * sidtimeDecimalHours
  where
    d
    {-- need the 0.5 to get from modified julian date to reduced julian date --} = fracDays ut -
                                                                                   fromIntegral
                                                                                     time20000101 -
                                                                                   0.5
    time20000101 = toModifiedJulianDay $ fromGregorian 2000 1 1
    sidtimeDecimalHours = (18.697374558 + 24.06570982441908 * d) `mod'` 24

timeFromSidereal :: UTCTime -> Deg -> UTCTime
timeFromSidereal fromTime sTime =
  addUTCTime (realToFrac dt) fromTime
    where
      st0 = siderealtime fromTime
      d' = sTime - st0
      d  = if d' < 0 then d'+360 else d'
      (Degrees dt) = 24 * 10 * (24/24.06570982441908) * d

currentSiderealtime :: IO Deg
currentSiderealtime = siderealtime <$> getCurrentTime

currentLocalSiderealtime :: GeoLoc -> IO Deg
currentLocalSiderealtime l = localSiderealtime l <$> getCurrentTime

localSiderealtime :: GeoLoc -> UTCTime -> Deg
localSiderealtime (GeoLoc _ long) ut = Degrees $ lst `mod'` 360
  where
    (Degrees lst) = siderealtime ut + long

-- Give location, a date a local siderealtime and
-- we will give the next point in time with given local siderealtime time
localSiderealtimeToUtcTime :: GeoLoc -> UTCTime -> Deg -> UTCTime
localSiderealtimeToUtcTime (GeoLoc _ long) ut lst =
  timeFromSidereal ut (lst - long)

toHorPosCoord :: Deg -> GeoLoc -> EqPos -> HorPos
toHorPosCoord lst (GeoLoc fi z) (EqPos ra d) = HorPos az al
  where
    lha = lst - ra
    al = altitude lst (GeoLoc fi z) (EqPos ra d)
    azy = - sine lha * cosine d / cosine al
    azx = (sine d - sine fi * sine al) / (cosine fi * cosine al)
    az = degrees $ solveAngle azx azy

altitude :: Deg -> GeoLoc -> EqPos -> Deg
altitude lst (GeoLoc fi _) (EqPos ra d) = al
  where
    lha = lst - ra
    al = arcsine $ sine d * sine fi + cosine d * cosine fi * cosine lha

equatorialToHorizontal :: GeoLoc -> UTCTime -> EqPos -> HorPos
equatorialToHorizontal loc ut = toHorPosCoord lst loc
  where
    lst = localSiderealtime loc ut

toEqPosCoord :: Deg -> GeoLoc -> HorPos -> EqPos
toEqPosCoord  lst (GeoLoc fi _) (HorPos az al) = EqPos ra d
  where
    d = arcsine $ sine al * sine fi + cosine al * cosine fi * cosine az
    lha = degrees $ solveAngle lhax lhay
    lhay = - sine az * cosine al / cosine d
    lhax = (sine al - sine d * sine fi) / (cosine d * cosine fi)
    ra' = lst - lha
    ra = if ra' < 0 then ra' + 360 else ra'

horizontalToEquatorial :: GeoLoc -> UTCTime -> HorPos -> EqPos
horizontalToEquatorial  loc ut = toEqPosCoord lst loc
  where
    lst = localSiderealtime loc ut

findNear :: [SkyObject] -> EqPos -> Double -> Maybe SkyObject
findNear ss eq d = listToMaybe [ s | (sd, s) <- [closest], sd < d]
  where
    distances = zip (fmap (dis eq . equatorial) ss) ss
    closest = Map.findMin $ Map.fromList distances
    dis (EqPos x y) (EqPos x' y') = vmag $ cartesian (HorPos x y, 1) - cartesian (HorPos x' y', 1)

visibleInNow :: GeoLoc -> Float -> Rectangle -> IO [(SkyObject, HorPos)]
visibleInNow geo minMag r =
   do
     t <- getCurrentTime
     return $ visibleIn geo minMag r t

visibleIn :: GeoLoc -> Float -> Rectangle -> UTCTime -> [(SkyObject, HorPos)]
visibleIn geo minMag r t =
    let f so = equatorialToHorizontal geo t (equatorial so);
        sos = brightSkyObjects minMag
    in filter (\(_, h) -> viewingRestriction r h) (zip sos (fmap f sos))

viewingRestriction :: Rectangle -> HorPos -> Bool
viewingRestriction r (HorPos a h) =
  let azimuthRes = if r^.minAz > r^.maxAz
      then
        ((r^.minAz <= a) && (a <= 360))
        || ((0 <= a) && (a <= r^.maxAz))
      else
        (r^.minAz <= a) &&
        (r^.maxAz >= a);   altitudeRes = (r^.minAl <= h) &&  (r^.maxAl >= h);
         in azimuthRes && altitudeRes

tour :: GeoLoc -> Float -> Rectangle -> UTCTime -> Int -> [(UTCTime, SkyObject, HorPos)]
tour g m r t _ = (\(desc, pos) -> (t, desc, pos)) <$> visibleIn g m r t

printDeg :: Deg -> String
printDeg deg = printf "%d\x00B0 %d\"%d'" d m s
  where
    (d, m, s) = toMinutesSeconds deg

pretty :: TimeZone -> (UTCTime, SkyObject, HorPos) -> String
pretty lz (t, so, HorPos a h) =
  printf "%s  %s\tAzi: %s\tAlt: %s" tf (description so) (printDeg a) (printDeg h)
  where
    tf = formatTime defaultTimeLocale "%X" lt
    lt = utcToLocalTime lz t

-- for every obj calculate the position for an interval from time t for d seconds and a frame every n seconds.
-- get the best position (TODO mark it as absolute or time induced maximum)
-- sort the set by time and altitude



timeInterval :: Integer -> Integer -> Integer -> [Integer]
timeInterval t d n = if d >= 0 then t : timeInterval (t + n) (d - n) n
                               else []

utctimeInterval :: UTCTime -> Integer -> Integer -> [UTCTime]
utctimeInterval t d n = fmap (posixSecondsToUTCTime . fromInteger) (timeInterval ((round . utcTimeToPOSIXSeconds) t) d n)

-- Want to solve t given an azimuth value or altitude value.
-- (i)   t = H + α
-- (ii)  sin(a) = sin(δ) sin(φ) + cos(δ) cos(φ) cos(H)
-- (iii) sin(A) = - sin(H) cos(δ) / cos(a) ==> cos(a) = - sin(H) cos(δ) /  sin(A)
-- (iv)  cos(A) = { sin(δ) - sin(φ) sin(a) } / cos(φ) cos(a)
--
-- (v)   sin(δ) = sin(a)sin(φ) + cos(a) cos(φ) cos(A)
-- (vi)  sin(H) = - sin(A) cos(a) / cos(δ)
-- (vii) cos(H) = { sin(a) - sin(δ) sin(φ)} / cos(δ) cos(φ)
--
-- Given A sin x + B cos x + C = 0 returns values for x
solveTrigonom :: Double -> Double -> Double -> [Deg]
solveTrigonom a b c = if d >= 0 then result else []
  where
    d = 4 * a * a - 4 * (c - b) * (b + c)
    result = fmap ((2 *) . arctangent . (\x -> x/(2 * c  - 2 * b))) [-2 * a + sqrt d , -2 * a - sqrt d ]

heightForAzimuth :: GeoLoc -> EqPos -> Deg ->  [Deg]
heightForAzimuth (GeoLoc fi _)(EqPos _ dec) az =
  -- sin(δ) = sin(a)sin(φ) + cos(a) cos(φ) cos(A) =>
  -- sin(a) sin(φ) + cos(a) cos(φ) cos(A) - sin(δ) = 0
  filter (\x -> x <= 90 && x >= -90) $ solveTrigonom ( sine fi) (cosine fi * cosine az) (- sine dec)

transitPos :: GeoLoc -> EqPos -> HorPos
transitPos geo eq = let nh = maximum (heightForAzimuth geo eq 0 `mappend` [-100]);
                           sh = maximum(heightForAzimuth geo eq 180 `mappend` [-100]);
                           in if nh > sh then HorPos 0 nh else HorPos 180 sh

lowestPos :: GeoLoc -> EqPos -> HorPos
lowestPos geo eq = let nh = minimum (heightForAzimuth geo eq 0 `mappend` [100]);
                           sh = minimum(heightForAzimuth geo eq 180 `mappend` [100]);
                           in if nh < sh then HorPos 0 nh else HorPos 180 sh

azimuthForHeight :: GeoLoc -> EqPos -> Deg ->  [Deg]
azimuthForHeight (GeoLoc fi _)(EqPos _ dec) a =
  -- cos az = { sin(δ) - sin(φ) sin(a) } / cos(φ) cos(a)
  let x = (sine dec - sine fi * sine a) / (cosine fi * cosine a);
    in if abs x <= 1
        then let t = arccosine x in [t, 360- t]
      else
        []

localSiderealtimeFromPos :: GeoLoc -> EqPos -> HorPos -> Deg
localSiderealtimeFromPos (GeoLoc fi _)(EqPos ra dec) (HorPos az a) = f
-- (i)   t = H + α
--
-- (vi)  sin(H) = - sin(A) cos(a) / cos(δ)
-- (vii) cos(H) = { sin(a) - sin(δ) sin(φ)} / cos(δ) cos(φ)
  where
    f = g . degrees $
     solveAngle
     ((sine a - sine dec * sine fi) / (cosine dec * cosine fi))
      (- sine az * cosine a / cosine dec)
    g x = let s = x + ra in if s >= 360 then s - 360 else s

intersectAzimuth :: GeoLoc -> EqPos -> Deg ->  [(Deg, HorPos)]
intersectAzimuth g eq az =
  do
    a <- heightForAzimuth g eq az
    let ps = HorPos az a
    return (localSiderealtimeFromPos g eq ps, ps)

intersectHeight :: GeoLoc -> EqPos -> Deg ->  [(Deg, HorPos)]
intersectHeight g eq a =
  do
    az <- azimuthForHeight g eq a
    let ps = HorPos az a
    return (localSiderealtimeFromPos g eq ps, ps)

crossingRect :: GeoLoc -> Rectangle -> EqPos -> [((Deg, HorPos), Bool)]
crossingRect geo r eq =  zip rs (fmap (\(s, _) -> laterIn s) rs)
  where
    f = filter (viewingRestriction r . snd)
    hminis = f $ intersectHeight geo eq (r^.minAl)
    hmaxis = f $ intersectHeight geo eq (r^.maxAl)
    azminis = f $ intersectAzimuth geo eq (r^.minAz)
    azmaxis = f $ intersectAzimuth geo eq  (r^.maxAz)
    rs = sortWith fst $  concat [hminis, hmaxis, azminis, azmaxis]
    testPos s = toHorPosCoord  (s + 0.001) geo eq
    laterIn s = viewingRestriction r (testPos s)

helper :: [(a, Bool)] -> [(a, Bool)]
helper l =
   case l of [] -> []
             h:xs -> if snd h then h:xs else xs `mappend` [h]

passages :: GeoLoc -> Rectangle -> EqPos -> [(((Deg, HorPos), (Deg, HorPos)), (Deg, (Deg, Deg)))]
passages geo r eq = zip l $ fmap (minh &&& maxh &&& timeDelta) l
  where
    crss = crossingRect geo r eq
    c = helper crss
    l = zip (fst <$> filter snd c) (fst <$> filter (not . snd) c)
    minh p = min (snd (fst p)^.hAltitude) (snd(snd p)^.hAltitude)
    maxh p = max (snd (fst p)^.hAltitude) (snd(snd p)^.hAltitude)
    timeDelta p = (/ 15)  (let t = fst (snd p) - fst (fst p)
                      in if t < 0 then t + 360 else t)

testDisplayPassages ::  GeoLoc -> Rectangle -> EqPos -> IO ()
testDisplayPassages geo r eq = mapM_  print  $ passages geo r eq

-- Want for any SkyObject, geo, viewing rectangle
-- * passages if any
-- * min height and max height
-- * total amount of time the object is visible

data Direction = DTop|DBottom|DLeft|DRight
  deriving (Show, Eq)

data ViewingReport =
  ViewingReport{
   _vPassages  :: [((Deg, HorPos, Direction), (Deg, HorPos, Direction))],
   _vMinHeight :: Deg,
   _vMaxHeight :: Deg
} deriving Show

makeLenses ''ViewingReport

createViewingReport :: GeoLoc -> Rectangle -> EqPos -> Maybe ViewingReport
createViewingReport geo r eq = if noAnswer then Nothing else
                                          Just $ ViewingReport ps minh maxh
  where
    hminis = h minAl DBottom
    hmaxis = h maxAl DTop
    azminis = a minAz DLeft
    azmaxis = a maxAz DRight

    h l s = zip ( filter (viewingRestriction r . snd) $ intersectHeight geo eq  (r^.l)) (repeat s)
    a l s = zip (filter (viewingRestriction r . snd) $ intersectAzimuth geo eq  (r^.l)) (repeat s)

    rs' = sortWith (fst.fst) $  concat [hminis, hmaxis, azminis, azmaxis]
    rs = fmap (\((x, y), z) -> (x, y, z)) rs'


    minh =
      let lp = lowestPos geo eq in
        if viewingRestriction r lp
          then lp ^. hAltitude
          else minimum $ fmap (\(_,HorPos _ al,_) -> al) rs

    maxh =
      let tp = transitPos geo eq in
        if viewingRestriction r tp
          then tp ^. hAltitude
          else maximum $ fmap (\(_,HorPos _ al,_) -> al) rs

    trs =  helper $ zip rs (fmap (\(s,_,_) -> laterIn s) rs)
    testPos s = toHorPosCoord  (s + 0.001) geo eq
    laterIn s = viewingRestriction r (testPos s)

    ps = zip (fst <$> filter snd trs) (fst <$> filter (not . snd) trs)

    noAnswer = null ps && not (viewingRestriction r (lowestPos geo eq)) && not (viewingRestriction r (transitPos geo eq))

intersectInterval :: (Deg,Deg) -> (Deg,Deg) -> Bool
intersectInterval (t0,t1) (u0,u1) | t0 < t1 && u0 < u1 = (t0 < u1 && u1 < t1) || (t0 < u0 && u0 < t1) || (u0 < t0 && t0 < u1)
                                                         || (u0 < t1 && t1 < u1)
                                  | t0 > t1 && u0 < u1 =
                                    intersectInterval (t0, 360) (u0, u1) ||
                                      intersectInterval (0, t1) (u0, u1)
                                  | t0 > t1 && u0 > u1 =
                                    intersectInterval (t0, 360) (u0, 360) ||
                                      intersectInterval (0, t1) (u0, 360) ||
                                        intersectInterval (t0, 360) (0, u1) ||
                                          intersectInterval (0, t1) (0, u1)
                                  | t0 < t1 && u0 > u1 =
                                    intersectInterval (t0, t1) (u0, 360) ||
                                      intersectInterval (t0, t1) (0, u1)
                                  | otherwise = False

relevant:: GeoLoc -> Rectangle -> (Deg, Deg) -> EqPos -> Bool
relevant geo r (t0, t1) eq =
  case vr of Nothing -> False
             Just rep -> null (rep^.vPassages) || any (\((x,_,_),(y,_,_)) -> intersectInterval (t0,t1) (x,y)) (rep^.vPassages)

  -- union of ViewingReport without passages (circum polair) and
  -- Exits passage such that eq is visible for some time, i. e.
  -- the intervals have a non-empty intersection.
  where
    vr = createViewingReport geo r eq

bestPosition:: GeoLoc -> Rectangle -> UTCTime -> Integer -> Integer -> SkyObject -> Maybe(UTCTime, SkyObject, HorPos)
bestPosition geo r t d n so = if relevant geo r (lst0, lst1) (equatorial so)
   then
    listToMaybe res
   else
     Nothing
  where
    f t' = (t', so, equatorialToHorizontal geo t' (equatorial so));
    pos = fmap f (utctimeInterval t d n)
    -- descending on altitude
    res = reverse . sortWith (\(_, _, HorPos _ x) -> x) $ filter (\(_,_, h) -> viewingRestriction r h) pos
    lst0 = localSiderealtime geo t
    lst1 = localSiderealtime geo (addUTCTime (fromInteger d) t)

tour2 :: GeoLoc -> Float -> Rectangle -> UTCTime -> Integer -> Integer -> [(UTCTime, SkyObject, HorPos)]
tour2 g m r t d n = sortWith (\(s,_,_)->s) $ mapMaybe (bestPosition g r t d n) objects
    where
      objects = brightNGCObjects m

viewTourNow :: GeoLoc -> Float -> Rectangle -> Integer -> Integer -> IO ()
viewTourNow g m r d n =
  do
    t <- getCurrentTime
    lz <- getCurrentTimeZone
    mapM_ (putStrLn . pretty lz) $ tour2 g m r t d n
