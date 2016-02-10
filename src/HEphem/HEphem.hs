{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | http://star-www.st-and.ac.uk/~fv/webnotes/chapter7.htm
module HEphem.HEphem where

import           Data.Angle
import           Data.Fixed            (mod')
import           Data.List
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Data.Vector.Class
import           HEphem.BSParser
import           HEphem.Data
import           HEphem.NGCParser
import           HEphem.ParserUtil

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
brightSkyObjects minMag = brightFilter minMag allSkyObjects

brightNGCObjects :: Float -> [SkyObject]
brightNGCObjects minMag = brightFilter minMag ngcSkyObjects

brightFilter :: Float -> [SkyObject] -> [SkyObject]
brightFilter m = filter (\s -> magnitude s < m)

siderealtime :: UTCTime -> Deg
siderealtime utc = Degrees $ 15 * sidtimeDecimalHours
  where
    d
    {-- need the 0.5 to get from modified julian date to reduced julian date --} = fracDays utc -
                                                                                   fromIntegral
                                                                                     time20000101 -
                                                                                   0.5
    time20000101 = toModifiedJulianDay $ fromGregorian 2000 1 1
    sidtimeDecimalHours = (18.697374558 + 24.06570982441908 * d) `mod'` 24

currentSiderealtime :: IO Deg
currentSiderealtime = siderealtime <$> getCurrentTime

currentLocalSiderealtime :: GeoLoc -> IO Deg
currentLocalSiderealtime l = localSiderealtime l <$> getCurrentTime

localSiderealtime :: GeoLoc -> UTCTime -> Deg
localSiderealtime (GeoLoc _ long) utc = Degrees $ lst `mod'` 360
  where
    (Degrees lst) = siderealtime utc + long


toHorPosCoord :: Deg -> GeoLoc -> EqPos -> HorPos
toHorPosCoord lst (GeoLoc fi _) (EqPos ra d) = HorPos az al
  where
    lha = lst - ra
    al = arcsine $ sine d * sine fi + cosine d * cosine fi * cosine lha
    azy = -sine lha * cosine d / cosine al
    azx = (sine d - sine fi * sine al) / (cosine fi * cosine al)
    az = degrees $ solveAngle azx azy

equatorialToHorizontal :: GeoLoc -> UTCTime -> EqPos -> HorPos
equatorialToHorizontal loc utc = toHorPosCoord lst loc
  where
    lst = localSiderealtime loc utc

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
horizontalToEquatorial  loc utc = toEqPosCoord lst loc
  where
    lst = localSiderealtime loc utc

findNear :: [SkyObject] -> EqPos -> Double -> Maybe SkyObject
findNear ss eq d = listToMaybe [ s | (sd, s) <- [closest], sd < d]
  where
    distances = zip (map (dis eq . equatorial) ss) ss
    closest = Map.findMin $ Map.fromList distances
    dis (EqPos x y) (EqPos x' y') = vmag $ cartesian (HorPos x y, 1) - cartesian (HorPos x' y', 1)

data Rectangle = Rectangle Deg Deg Deg Deg

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
viewingRestriction (Rectangle minAz maxAz minAl maxAl) (HorPos a h) =
  let azimuthRes = if minAz > maxAz
      then
        ((minAz <= a) && (a <= 360))
        || ((0 <= a) && (a <= maxAz))
      else
        (minAz <= a) &&
        (maxAz >= a);   altitudeRes = (minAl <= h) &&  (maxAl >= h);
         in azimuthRes && altitudeRes

tour :: GeoLoc -> Float -> Rectangle -> UTCTime -> Int -> [(UTCTime, SkyObject, HorPos)]
tour g m r t _ = map (\(desc, pos) -> (t, desc, pos)) $ visibleIn g m r t

viewTourNow :: GeoLoc -> Float -> Rectangle -> Integer -> Integer -> IO ()
viewTourNow g m r d n =
  do
    t <- getCurrentTime
    mapM_ (putStrLn . pretty) $ tour2 g m r t d n

pretty :: (UTCTime, SkyObject, HorPos) -> String
pretty (t, b, HorPos a h) =  formatTime defaultTimeLocale "%X" t ++ " " ++ description b ++
                         " Azi: " ++
                         show d ++
                         "\x00B0" ++
                         show m ++
                         "\"" ++
                         show s ++
                         "'" ++
                         " Alt: " ++
                         show d' ++
                         "\x00B0" ++
                         show m' ++
                         "\"" ++
                         show s' ++
                         "'"
  where
    (d, m, s) = toMinutesSeconds a
    (d', m', s') = toMinutesSeconds h

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
