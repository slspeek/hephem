{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | http://star-www.st-and.ac.uk/~fv/webnotes/chapter7.htm
module HEphem.HEphem where

import           Control.Arrow
import           Control.Lens          hiding (element)
import           Data.Angle
import           Data.Fixed            (mod')
import           Data.List
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
ngcSkyObjects = map NGC ngcObjectList

brightStarObjects :: [SkyObject]
brightStarObjects = map Star brightstarlist

allSkyObjects :: [SkyObject]
allSkyObjects = ngcSkyObjects ++ brightStarObjects

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
    distances = zip (map (dis eq . equatorial) ss) ss
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
    in filter (\(_, h) -> viewingRestriction r h) (zip sos (map f sos))

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
tour g m r t _ = map (\(desc, pos) -> (t, desc, pos)) $ visibleIn g m r t

viewTourNow :: GeoLoc -> Float -> Rectangle -> Integer -> Integer -> IO ()
viewTourNow g m r d n =
  do
    t <- getCurrentTime
    lz <- getCurrentTimeZone
    mapM_ (putStrLn . pretty lz) $ tour2 g m r t d n

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
tour2 :: GeoLoc -> Float -> Rectangle -> UTCTime -> Integer -> Integer -> [(UTCTime, SkyObject, HorPos)]
tour2 g m r t d n = sortBy comp $ mapMaybe (bestPosition g r t d n) objects
    where
      objects = brightNGCObjects m
      -- ascending on time
      comp (y, _, _ ) (x, _, _)
       | x < y = GT
       | y > x = LT
       | otherwise = EQ

bestPosition:: GeoLoc -> Rectangle -> UTCTime -> Integer -> Integer -> SkyObject -> Maybe(UTCTime, SkyObject, HorPos)
bestPosition geo r t d n so = listToMaybe res
  where
    f t' = (t', so, equatorialToHorizontal geo t' (equatorial so));
    pos = map f (utctimeInterval t d n)
    -- descending on altitude
    comp (_, _, HorPos _ x) (_, _, HorPos _ y)
     | x < y = GT
     | y > x = LT
     | otherwise = EQ
    res = sortBy comp $ filter (\(_,_, h) -> viewingRestriction r h) pos

timeInterval :: Integer -> Integer -> Integer -> [Integer]
timeInterval t d n = if d >= 0 then t : timeInterval (t + n) (d - n) n
                               else []

utctimeInterval :: UTCTime -> Integer -> Integer -> [UTCTime]
utctimeInterval t d n = map (posixSecondsToUTCTime . fromInteger) (timeInterval ((round . utcTimeToPOSIXSeconds) t) d n)

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
    result = map ((2 *) . arctangent . (\x -> x/(2 * c  - 2 * b))) [-2 * a + sqrt d , -2 * a - sqrt d ]

--Faulty
heightForAzimuth :: GeoLoc -> EqPos -> Deg ->  [Deg]
heightForAzimuth (GeoLoc fi _)(EqPos _ dec) az =
  -- sin(δ) = sin(a)sin(φ) + cos(a) cos(φ) cos(A) =>
  -- sin(a) sin(φ) + cos(a) cos(φ) cos(A) - sin(δ) = 0
  filter (<=90) $ solveTrigonom ( sine fi) (cosine fi * cosine az) (- sine dec)

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

--Faulty
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

testDisplayAzi :: GeoLoc -> EqPos -> Deg ->  IO [(UTCTime, HorPos)]
testDisplayAzi geo eq az =
  do
    t <- getCurrentTime
    return $ map (Control.Arrow.first (localSiderealtimeToUtcTime geo t)) $ intersectAzimuth geo eq az

testDisplayHeight :: GeoLoc -> EqPos -> Deg ->  IO [(UTCTime, HorPos)]
testDisplayHeight geo eq a =
  do
    t <- getCurrentTime
    return $ map (Control.Arrow.first (localSiderealtimeToUtcTime geo t)) $ intersectHeight geo eq a

--Faulty
crossingRect :: GeoLoc -> Rectangle -> EqPos -> [((Deg, HorPos), Bool)]
crossingRect geo r eq =  sortWith (fst . fst) $  hminis' ++  hmaxis' ++ azminis' ++ azmaxis'
  where
    f = filter (viewingRestriction r . snd)
    hminis = f $ intersectHeight geo eq (r^.minAl)
    hminis' = zip hminis (map (ascending . snd) hminis)

    hmaxis = f $ intersectHeight geo eq (r^.maxAl)
    hmaxis' = zip hmaxis (map (not . ascending . snd) hmaxis)

    azminis = f $ intersectAzimuth geo eq (r^.minAz)
    azminis' = zip azminis  (map (leftToRight geo. snd) azminis)

    azmaxis = f $ intersectAzimuth geo eq  (r^.maxAz)
    azmaxis' = zip azmaxis   (map (not . leftToRight geo . snd) azmaxis)

--Faulty
crossingRect' :: GeoLoc -> Rectangle -> EqPos -> [((Deg, HorPos), (HorPos, Bool))]
crossingRect' geo r eq =  zip rs (map (\(s, _) -> laterIn s) rs)
  where
    f = filter (viewingRestriction r . snd)
    hminis = f $ intersectHeight geo eq (r^.minAl)
    hmaxis = f $ intersectHeight geo eq (r^.maxAl)
    azminis = f $ intersectAzimuth geo eq (r^.minAz)
    azmaxis = f $ intersectAzimuth geo eq  (r^.maxAz)
    rs = sortWith fst $  hminis ++  hmaxis ++ azminis ++ azmaxis
    laterIn s = (toHorPosCoord  (s + 0.1) geo eq,  viewingRestriction r (toHorPosCoord  s geo eq))

ascending :: HorPos -> Bool
ascending (HorPos az _) | az >=0 && az <=180 = True
                          | otherwise         = False

-- Faulty nonsense
leftToRight :: GeoLoc -> HorPos -> Bool
leftToRight (GeoLoc fi _) (HorPos az al) | al >= abs fi && fi < 0 = True
                                         | al < abs fi && fi < 0 = False
                                         -- Northern hemisphere
                                         | az >= 90 && az <= 270 && al >= abs fi && fi >= 0 = False
                                         | az >= 90 && az <= 270 = True
                                         | az > 270 && az < 90 && al >= abs fi && fi >= 0 = False
                                         | al < abs fi && fi >= 0 = True
                                         | otherwise = undefined
