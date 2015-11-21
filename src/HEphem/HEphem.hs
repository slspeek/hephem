{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

module HEphem.HEphem where

import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Fixed (mod')
import           Data.Vector.V3
import           Data.Vector.Class ()
import           Data.Angle
import           HEphem.BSParser
import           Test.QuickCheck

geoAms :: GeoLoc
geoAms = GeoLoc (fromDMS 52 21 0) (fromDMS 4 51 59)

class AEq a where
  (=~) :: a -> a -> Bool

instance AEq Double where
  x =~ y = abs (x - y) < (1.0e-8 :: Double)

instance AEq HorPos where
  x =~ y = abs (hAzimuth x - hAzimuth y) < d && abs (hAltitude x - hAltitude y) < d
    where
      d = 0.1 :: Degrees Double

instance AEq Vector3 where
  v =~ w = abs (v3x v - v3x w) < d &&
           abs (v3y v - v3y w) < d &&
           abs (v3z v - v3z w) < d
    where
      d = 1.0e-8 :: Double

instance AEq Float where
  x =~ y = abs (x - y) < (1.0e-4 :: Float)

instance (AEq a) => AEq (Degrees a) where
  (Degrees x) =~ (Degrees y) = x =~ y

instance (AEq a) => AEq (Radians a) where
  (Radians x) =~ (Radians y) = x =~ y

data GeoLoc = GeoLoc { gLatitude,gLongitude :: Deg }
  deriving (Eq, Show)

data HorPos = HorPos { hAzimuth,hAltitude :: Deg }
  deriving (Eq, Show)

instance Arbitrary HorPos where
  arbitrary = do
    az <- suchThat arbitrary (\x -> x >= 0 && x <= 360)
    al <- suchThat arbitrary (\x -> x >= 0 && x <= 90)
    return $ HorPos (Degrees az) (Degrees al)

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

horizontal :: GeoLoc -> UTCTime -> BrightStar -> (BrightStar, HorPos)
horizontal loc utc b = (b, toHorPosCoord lst loc (bEquatorial b))
  where
    lst = localSiderealtime loc utc

data Rectangle = Rectangle Deg Deg Deg Deg

visibleIn :: GeoLoc -> Rectangle -> IO [(BrightStar, HorPos)]
visibleIn geo (Rectangle minAz maxAz minAl maxAl) =
  do
    t <- getCurrentTime
    let f = horizontal geo t
    return $ filter p (map f brightstarlist)

  where
    p (_, HorPos a h) = (minAz <= a) &&
                        (maxAz >= a) &&
                        (minAl <= h) &&
                        (maxAl >= h)

pretty :: (BrightStar, HorPos) -> String
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

