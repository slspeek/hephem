{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HEphem.HEphem where

import           Data.Angle
import           Data.Fixed         (mod')
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Vector.Class  ()
import           HEphem.BSParser
import           HEphem.Data
import           HEphem.ParserUtil

geoAms :: GeoLoc
geoAms = GeoLoc (fromDMS 52 21 0) (fromDMS 4 51 59)

fracDays :: UTCTime -> Double
fracDays u = (fromIntegral . toModifiedJulianDay) (utctDay u) + (fromRational
                                                                   (toRational (utctDayTime u)) / 86400)

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

{--| Given cos A and sin A solve A --}
solveAngle :: Double -> Double -> Radians Double
solveAngle c s
  | s > 0 = arccosine c
  | c > 0 = Radians (2 * pi) + arcsine s
  | otherwise = Radians (2 * pi) - arccosine c

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

horizontalToEquatorial :: GeoLoc -> UTCTime -> HorPos -> EqPos
horizontalToEquatorial  = undefined

data Rectangle = Rectangle Deg Deg Deg Deg

visibleIn :: GeoLoc -> Rectangle -> IO [(SkyObject, HorPos)]
visibleIn geo (Rectangle minAz maxAz minAl maxAl) =
  do
    t <- getCurrentTime
    let f so = equatorialToHorizontal geo t (equatorial so)
    return $ filter p (zip brightstarlist (map f brightstarlist))

  where
    p (_, HorPos a h) = (minAz <= a) &&
                        (maxAz >= a) &&
                        (minAl <= h) &&
                        (maxAl >= h)

pretty :: (SkyObject, HorPos) -> String
pretty (b, HorPos a h) = bName b ++
                         " " ++
                         show (bMagitude b) ++
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
