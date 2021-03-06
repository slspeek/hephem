{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | http://star-www.st-and.ac.uk/~fv/webnotes/chapter7.htm
module HEphem.HEphem where

import           Control.Lens          hiding (element)
import           Data.Angle
import           Data.Fixed            (mod')
import           Data.List
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Ord
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Tuple.HT         (fst3)
import           Data.Vector.Class
import           GHC.Exts
import           HEphem.BSParser
import           HEphem.Data
import           HEphem.NGCParser
import           Text.Printf

geoAms :: GeoLoc
geoAms = GeoLoc (fromDMS 52 21 0) (fromDMS 4 51 59)

fracDays :: UTCTime -> Double
fracDays u = (fromIntegral . toModifiedJulianDay) (utctDay u)
                + (fromRational (toRational (utctDayTime u)) / 86400)

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

daySiderealDayRatio :: Double
daySiderealDayRatio = 24.06570982441908/24

siderealtime :: UTCTime -> Deg
siderealtime ut = Degrees $ 15 * sidtimeDecimalHours
  where
    -- need the 0.5 to get from modified julian date to reduced julian date --
    d = fracDays ut - fromIntegral time20000101 - 0.5
    time20000101 = toModifiedJulianDay $ fromGregorian 2000 1 1
    sidtimeDecimalHours = (18.697374558 + daySiderealDayRatio * 24 * d) `mod'` 24

timeFromSidereal :: UTCTime -> Deg -> UTCTime
timeFromSidereal fromTime sTime =
  addUTCTime (realToFrac dt) fromTime
    where
      st0 = siderealtime fromTime
      d = standardizeDeg $ sTime - st0
      (Degrees dt) = 24 * 10 *  d  * Degrees (1/ daySiderealDayRatio)

localSiderealtime :: GeoLoc -> UTCTime -> Deg
localSiderealtime (GeoLoc _ long) ut = standardizeDeg $ siderealtime ut + long

-- Given a location, a date and a local siderealtime and
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
    ra = standardizeDeg $ lst - lha

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

isInInterval:: Deg -> Interval -> Bool
isInInterval a (t0, t1) =   if t0 > t1
      then
        ((t0 <= a) && (a <= 360))
        || ((0 <= a) && (a <= t1))
      else
        (t0 <= a) && (a <= t1)

viewingRestriction :: Rectangle -> HorPos -> Bool
viewingRestriction r (HorPos a h) =
  isInInterval  a (r^.rAzimuth) && isInInterval h (r^.rAltitude)

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

-- Want for any SkyObject, geo, viewing rectangle
-- * passages if any
-- * min height and max height

data Direction = DTop|DBottom|DLeft|DRight
  deriving (Show, Eq)

data ViewingReport =
  ViewingReport{
   _vPassages  :: [((Deg, HorPos, Direction), (Deg, HorPos, Direction))],
   _vMinHeight :: (Deg, HorPos),
   _vMaxHeight :: (Deg, HorPos)
} deriving Show

makeLenses ''ViewingReport

data ViewOps =
  ViewOps{
  _oGeo        :: GeoLoc,
  _oMag        :: Float,
  _oRectangle  :: Rectangle,
  _oStart      :: UTCTime,
  _oDuration   :: Int,
  _oMinObsTime :: Int,
  _oMinScore   :: Int
} deriving (Show)

makeLenses ''ViewOps

data Report =
  Report{
  _rObject :: SkyObject,
  _rTime   :: UTCTime,
  _rHorPos :: HorPos,
  _rScore  :: Double,
  _rBefore :: Deg,
  _rAfter  :: Deg,
  _rLST    :: Deg
}

makeLenses ''Report

rectangleIntersections :: GeoLoc -> Rectangle -> EqPos -> [((Deg, HorPos, Direction), Bool)]
rectangleIntersections geo r eq = trs
  where
    hminis = relevantMarkedIntersection intersectHeight (fst(r^.rAltitude)) DBottom
    hmaxis = relevantMarkedIntersection intersectHeight (snd(r^.rAltitude)) DTop
    azminis = relevantMarkedIntersection intersectAzimuth (fst(r^.rAzimuth)) DLeft
    azmaxis = relevantMarkedIntersection intersectAzimuth (snd(r^.rAzimuth)) DRight

    relevantMarkedIntersection method value marker =
      zip (filter (viewingRestriction r . snd) $ method geo eq value) (repeat marker)

    -- all marked intersections (lst, HorPos, Direction) sorted by local siderealtime
    rs = sortWith fst3
            ((\((x, y), z) -> (x, y, z))
              <$> concat [hminis, hmaxis, azminis, azmaxis])

    -- zip with the bool indicating entering or exiting the rectangle
    -- by calculating a position a little later
    trs =  helper $ zip rs (fmap (\(s,_,_) -> laterIn s) rs)
    laterIn s = viewingRestriction r (toHorPosCoord  (s + 0.001) geo eq)

    -- helper puts an exit at the end if one found at the head
    helper :: [(a, Bool)] -> [(a, Bool)]
    helper l =
       case l of [] -> []
                 h:xs -> if snd h then h:xs else xs `mappend` [h]



createViewingReport :: GeoLoc -> Rectangle -> EqPos -> Maybe ViewingReport
createViewingReport geo r eq = if noAnswer then Nothing else
                                          Just $ ViewingReport ps minh maxh
  where
    -- No ViewingReport when there are no passages
    -- and both lowest position and highest position lay outside the rectangle
    noAnswer = null ps
                && not (viewingRestriction r (lowestPos geo eq))
                  && not (viewingRestriction r (transitPos geo eq))

    is = rectangleIntersections geo r eq
    ps = zip (fst <$> filter snd is) (fst <$> filter (not . snd) is)

    inters = (\((s,hp,_), _) -> (s,hp)) <$> is
    minh =
      let lp = lowestPos geo eq in
        if viewingRestriction r lp
          then (localSiderealtimeFromPos geo eq lp, lp)
          else minimumBy (comparing snd) inters

    maxh =
      let tp = transitPos geo eq in
        if viewingRestriction r tp
          then (localSiderealtimeFromPos geo eq tp, tp)
          else maximumBy (comparing snd) inters



joinAdjacentIntervals:: [(Deg,Deg)] -> [(Deg,Deg)]
joinAdjacentIntervals [] = []
joinAdjacentIntervals ((t0, t1):(u0, u1):xs) = if standardizeDeg (t1 - u0) == 0 then joinAdjacentIntervals $ (t0, u1):xs
                                            else (t0, t1) : joinAdjacentIntervals ((u0, u1):xs)
joinAdjacentIntervals [(t0, t1)] = [(t0, t1)]

intersectInterval :: Interval -> Interval -> [Interval]
intersectInterval (t0,t1) (u0,u1) | t0 <= t1 && u0 <= u1 =
                                    let maxStart = max t0 u0; minEnd = min t1 u1;
                                    in [(maxStart, minEnd) | maxStart <= minEnd]
                                  | t0 > t1 && u0 < u1 =
                                    joinAdjacentIntervals
                                     (intersectInterval (t0, 360) (u0, u1)
                                      ++ intersectInterval (0, t1) (u0, u1))
                                  | t0 > t1 && u0 > u1 =
                                    joinAdjacentIntervals
                                      (intersectInterval (t0, 360) (u0, 360)
                                        ++ intersectInterval (0, t1) (0, u1)
                                          ++ intersectInterval (0, t1) (u0, 360)
                                           ++ intersectInterval (t0, 360) (0, u1))
                                  | t0 < t1 && u0 > u1 =
                                    joinAdjacentIntervals
                                     (intersectInterval (t0, t1) (u0, 360)
                                      ++ intersectInterval (t0, t1) (0, u1))
                                  | otherwise = []

bestPosition:: ViewOps -> SkyObject-> Maybe Report
bestPosition ops so =
     do
       vr <- createViewingReport geo (ops^.oRectangle) (equatorial so)
       ((lst, hp), score, tb, ta) <- bestPosition2 ops (equatorial so) (lst0, lst1) vr
       return $ Report
          so
            (localSiderealtimeToUtcTime geo t lst)
              hp
                (undeg score)
                  (toUTCDiff tb)
                    (toUTCDiff ta)
                      lst
  where
    geo = ops^.oGeo
    t = ops^.oStart
    lst0 = localSiderealtime geo t
    lst1 = localSiderealtime geo (addUTCTime (fromIntegral (ops^.oDuration)) t)
    toUTCDiff :: Deg -> Deg
    toUTCDiff sidD =  sidD / Degrees daySiderealDayRatio


bestPosition2:: ViewOps ->  EqPos -> Interval ->  ViewingReport -> Maybe ((Deg, HorPos), Deg, Deg, Deg)
bestPosition2 ops eq (lst0,lst1) vr = hmax
  where
    score x = let minH = snd (vr^.vMinHeight)^.hAltitude ;
                    maxH = snd (vr^.vMaxHeight)^.hAltitude
                    in 100 * (x - minH)/ (maxH - minH)

    interss = concatMap (\(x,y) -> intersectInterval (lst0, lst1) (fst3 x, fst3 y)) (vr^.vPassages)

    hor t = toHorPosCoord t (ops^.oGeo) eq
    borderPoss = map (\(s0, s1) -> ((s0, hor s0), (s1, hor s1))) interss

    m = maximumBy $ comparing snd

    highestInterval = fst $ maximumBy (comparing snd) (map (\z -> (z, m z)) borderPoss)

    transitInterval = filter (isInInterval (fst (vr^.vMaxHeight))) interss

    maxAtBegin x = snd (fst x) >= snd (snd x)
    hDuration = fst (snd highestInterval) - fst (fst highestInterval)

    hmax = if null transitInterval
      then if not (null interss)
        then
          if maxAtBegin highestInterval
            then
              return $ correctForMinObsTime (fst highestInterval, score (snd (fst highestInterval)^.hAltitude)
                , 0, hDuration)
            else
              return $ correctForMinObsTime (snd highestInterval, score (snd (snd highestInterval)^.hAltitude)
                , hDuration, 0)
          else Nothing
      else
        return $ correctForMinObsTime (vr^.vMaxHeight, 100, standardizeDeg (fst (vr^.vMaxHeight) - fst (fst highestInterval)),
                                 standardizeDeg (fst (snd highestInterval) - fst (vr^.vMaxHeight)))

    correctForMinObsTime   :: ((Deg, HorPos), Deg, Deg, Deg) -> ((Deg, HorPos), Deg, Deg, Deg)
    correctForMinObsTime  ((l, hp), s , tb, ta) =
          let watchingTime = min (tb - ta) (Degrees daySiderealDayRatio * 15 * fromIntegral (ops^.oMinObsTime) /3600);
              lst = l - (watchingTime - ta)
              res = (lst, hor lst)
          in if ta < watchingTime
            then
              (res, score (hor lst ^.hAltitude), tb - (watchingTime - ta), ta + watchingTime)
            else
              ((l, hp), s , tb, ta)


tour :: ViewOps -> [Report]
tour ops =
  filter (\report -> (report^.rScore) > fromIntegral (ops^.oMinScore))
   . sortWith (^.rTime) $ mapMaybe (bestPosition ops) objects
    where
      objects = brightSkyObjects (ops^.oMag)

pretty :: TimeZone -> Report -> String
pretty lz rep =
  printf "%s  %s\tAzi: %s\tAlt: %s\tScore: %.2f\tBefore: %s After: %s"
    tf
     (description (rep^.rObject)) (printDeg (rep^.rHorPos.hAzimuth)) (printDeg (rep^.rHorPos.hAltitude))
     (rep^.rScore) (printDegAsTime (rep^.rBefore)) (printDegAsTime (rep^.rAfter))
  where
    tf = formatTime defaultTimeLocale "%X" lt
    lt = utcToLocalTime lz (rep^.rTime)

viewTourNow :: ViewOps -> IO ()
viewTourNow ops =
  do
    lz <- getCurrentTimeZone
    mapM_ (putStrLn . pretty lz)  $ tour ops
