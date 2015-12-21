{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | http://star-www.st-and.ac.uk/~fv/webnotes/chapter7.htm
module HEphem.HEphem where

import           Data.Angle
import           Data.Fixed         (mod')
import qualified Data.Map           as Map
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
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

allSkyObjects :: [SkyObject]
allSkyObjects = map Star brightstarlist ++ map NGC ngcObjectList

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

visibleIn :: GeoLoc -> Rectangle -> IO [(SkyObject, HorPos)]
visibleIn geo (Rectangle minAz maxAz minAl maxAl) =
  do
    t <- getCurrentTime
    let f so = equatorialToHorizontal geo t (equatorial so)
    return $ filter p (zip allSkyObjects (map f allSkyObjects))

  where
    p (_, HorPos a h) = (minAz <= a) &&
                        (maxAz >= a) &&
                        (minAl <= h) &&
                        (maxAl >= h)

pretty :: (SkyObject, HorPos) -> String
pretty (b, HorPos a h) =  show (magnitude b) ++
                         " Azi: " ++
                         show d ++
                         "*" ++
                         show m ++
                         "\"" ++
                         show s ++
                         "'" ++
                         " Alt: " ++
                         show d' ++
                         "*" ++
                         show m' ++
                         "\"" ++
                         show s' ++
                         "'"
  where
    (d, m, s) = toMinutesSeconds a
    (d', m', s') = toMinutesSeconds h
