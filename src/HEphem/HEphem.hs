{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-}

module HEphem.HEphem where

import           Text.ParserCombinators.ReadP
import           Data.String
import           Data.FileEmbed
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Fixed (mod', div')
import           Data.Vector.V3
import           Data.Vector.Class
import qualified Graphics.Gloss.Data.Point as P
import           GHC.Float
import           Data.Vector.Fancy
import           Data.Angle
import           Data.Vector.Transform.T3
import           Data.Vector.Transform.Fancy
import           Test.QuickCheck
import           Control.Monad

geoAms :: GeoLocation
geoAms = GeoLocation (Latitude 52 21 0) (Longitude 4 51 59)

brightstartext :: IsString a => a
brightstartext = $(embedStringFile "brightstar_2015/brightstar_2015.txt")

brightstarlist :: [BrightStar]
brightstarlist =
  let starlines = (drop 5 . lines) brightstartext
  in map (fst . last . readP_to_S star) starlines

data BrightStar =
       BrightStar
         { bName :: String
         , bHRNo :: Int
         , bRA :: RA
         , bDec :: Dec
         , bNotes :: String
         , bMagitude :: Double
         , bUminB :: Maybe Double
         , bBminV :: Double
         , bSpectralType :: String
         }
  deriving (Eq, Show)

star :: ReadP BrightStar
star = do
  name <- readName
  _ <- space
  hr <- readHRNo
  _ <- space
  ra <- readRA
  _ <- space
  d <- readDec
  _ <- space
  notes <- readNotes
  _ <- space
  mag <- readMagnitude
  _ <- space
  uminB <- readUminB
  _ <- space
  bminV <- readBminV
  _ <- space
  spec <- readSpectralType

  return
    BrightStar
      { bName = name
      , bHRNo = hr
      , bRA = ra
      , bDec = d
      , bNotes = notes
      , bMagitude = mag
      , bUminB = uminB
      , bBminV = bminV
      , bSpectralType = spec
      }

  where
    space = char ' '

getn :: Int -> ReadP String
getn n = count n get

readName :: ReadP String
readName = getn 19

readHRNo :: ReadP Int
readHRNo = do
  s <- getn 5
  return $ read s

readRA :: ReadP RA
readRA = do
  h <- getn 3
  let hi = read h
  m <- getn 3
  let mi = read m
  s <- getn 5
  let sf = read s
  return $ RA hi mi sf

replace :: String -> String
replace = map
            (\c -> if c == '+'
                     then ' '
                     else c)

readDec :: ReadP Dec
readDec = do
  h <- getn 5
  let h' = replace h
  let hi = read h'
  m <- getn 3
  let mi = read m
  s <- getn 3
  let sf = read s
  return $ Dec hi mi sf

readNotes :: ReadP String
readNotes = getn 8

readMagnitude :: ReadP Double
readMagnitude = do
  m <- getn 5
  return $ read m

readUminB :: ReadP (Maybe Double)
readUminB = do
  m <- getn 5
  if m == "     "
    then return Nothing
    else return . Just . read $ replace m

readBminV :: ReadP Double
readBminV = do
  m <- getn 5
  return . read $ replace m

readSpectralType :: ReadP String
readSpectralType = many1 get

bEquatorial :: BrightStar -> Equatorial
bEquatorial b = Equatorial (bRA b) (bDec b)

class HasAngle a where
  angle :: a -> Double

torad :: Int -> Int -> Double -> Double
torad d m s = deg2rad $ todec d m s

deg2rad :: Double -> Double
deg2rad d = pi / 180 * d

rad2deg :: Double -> Double
rad2deg r = 180 / pi * r

todec :: Int -> Int -> Double -> Double
todec d m s = fromIntegral d + ((fromIntegral m / 60.0) + (s / 3600.0))

class AEq a where
  (=~) :: a -> a -> Bool

instance AEq Double where
  x =~ y = abs (x - y) < (1.0e-8 :: Double)

instance AEq Horizontal where
  x =~ y = abs (xaz - yaz) < d && abs (xa - ya) < d
    where
      Horizontal (Azimuth xaz) (Altitude xa) = x
      Horizontal (Azimuth yaz) (Altitude ya) = y
      d = 0.1 :: Double

instance AEq Vector3 where
  v =~ w = abs (v3x v - v3x w) < d &&
           abs (v3y v - v3y w) < d &&
           abs (v3z v - v3z w) < d
    where
      d = 1.0e-8 :: Double

instance AEq Float where
  x =~ y = abs (x - y) < (1.0e-4 :: Float)

{-instance AEq (Vector3, Vector3) where-}
{-v =~ w = fst v =~ fst w && snd v =~ snvd w-}
data GeoLocation = GeoLocation Latitude Longitude
  deriving (Eq, Show)

data Latitude = Latitude Int Int Double
  deriving (Eq, Show)

instance HasAngle Latitude where
  angle (Latitude d m s) = torad d m s

data Longitude = Longitude Int Int Double
  deriving (Eq, Show)

data Equatorial = Equatorial RA Dec
  deriving (Eq, Show)

data RA = RA Int Int Double
  deriving (Eq, Show)

instance HasAngle RA where
  angle (RA h m s) = deg2rad (15 * todec h m s)

data Dec = Dec Int Int Double
  deriving (Eq, Show)

instance HasAngle Dec where
  angle (Dec d m s) = torad d m s

data Horizontal = Horizontal Azimuth Altitude
  deriving (Eq, Show)

data Azimuth = Azimuth Double
  deriving (Eq, Show)

instance HasAngle Azimuth where
  angle (Azimuth az) = deg2rad az

data Altitude = Altitude Double
  deriving (Eq, Show)

instance HasAngle Altitude where
  angle (Altitude h) = deg2rad h

instance Arbitrary Screen where
  arbitrary = liftM2 Screen arbitrary (suchThat arbitrary (\x -> x > 1))

instance Arbitrary Horizontal where
  arbitrary = liftM2 Horizontal arbitrary arbitrary

instance Arbitrary Azimuth where
  arbitrary = liftM Azimuth (suchThat arbitrary (\x -> x >= 0 && x <= 360))

instance Arbitrary Altitude where
  arbitrary = liftM Altitude (suchThat arbitrary (\x -> x >= 0 && x < 90))

instance Arbitrary Vector3 where
  arbitrary = liftM3 Vector3 arbitrary arbitrary arbitrary

rA :: Equatorial -> Double
rA (Equatorial ra _) = angle ra

dec :: Equatorial -> Double
dec (Equatorial _ d) = angle d

fracDays :: UTCTime -> Double
fracDays u = (fromIntegral . toModifiedJulianDay) (utctDay u) + (fromRational
                                                                   (toRational (utctDayTime u)) / 86400)

siderealtime :: UTCTime -> Double
siderealtime utc = (18.697374558 + 24.06570982441908 * d) `mod'` 24
  where
    d
    {-- need the 0.5 to get from modified julian date to reduced julian date --} = fracDays utc -
                                                                                   fromIntegral
                                                                                     time20000101 -
                                                                                   0.5
    time20000101 = toModifiedJulianDay $ fromGregorian 2000 1 1

currentSiderealtime :: IO Double
currentSiderealtime = siderealtime <$> getCurrentTime

currentLocalSiderealtime :: Longitude -> IO Double
currentLocalSiderealtime l = localSiderealtime l <$> getCurrentTime

toMinutesSeconds :: Double -> (Int, Int, Int)
toMinutesSeconds d = (i, m, s)
  where
    i = floor d
    r = d - fromIntegral i
    m = r `div'` (1 / 60)
    r' = r - fromIntegral m * (1 / 60)
    s = r' `div'` (1 / 3600)

localSiderealtime :: Longitude -> UTCTime -> Double
localSiderealtime (Longitude d m s) utc = (siderealtime utc + todec d m s / 15) `mod'` 24

{--| Given cos A and sin A solve A --}
solveAngle :: Double -> Double -> Double
solveAngle c s
  | s > 0 = acos c
  | c > 0 = 2 * pi + asin s
  | otherwise = 2 * pi - acos c

toHorizontalCoord :: Double -> Latitude -> Equatorial -> Horizontal
toHorizontalCoord lst fi (Equatorial ra d) = Horizontal (Azimuth az) (Altitude a)
  where
    lstr = deg2rad $ 15.0 * lst
    lha = lstr - angle ra
    dr = angle d
    fir = angle fi
    ar = asin $ sin dr * sin fir + cos dr * cos fir * cos lha
    azy = -sin lha * cos dr / cos ar
    azx = (sin dr - sin fir * sin ar) / (cos fir * cos ar)
    azr = solveAngle azx azy
    a = rad2deg ar
    az = rad2deg azr

horizontal :: GeoLocation -> UTCTime -> BrightStar -> (BrightStar, Horizontal)
horizontal (GeoLocation lat long) utc b = (b, toHorizontalCoord lst lat (bEquatorial b))
  where
    lst = localSiderealtime long utc

data Rectangle = Rectangle Azimuth Azimuth Altitude Altitude

visibleIn :: GeoLocation -> Rectangle -> IO [(BrightStar, Horizontal)]
visibleIn geo (Rectangle (Azimuth minAz) (Azimuth maxAz) (Altitude minAl) (Altitude maxAl)) =
  do
    t <- getCurrentTime
    let f = horizontal geo t
    return $ filter p (map f brightstarlist)

  where
    p (_, Horizontal (Azimuth a) (Altitude h)) = (minAz <= a) &&
                                                 (maxAz >= a) &&
                                                 (minAl <= h) &&
                                                 (maxAl >= h)

pretty :: (BrightStar, Horizontal) -> String
pretty (b, Horizontal (Azimuth a) (Altitude h)) = bName b ++
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

{-- For graphical representation --}
data World = World { wStars :: [BrightStar], wScreen :: Screen }
  deriving (Eq, Show)

{-- Viewing screen has a direction and distance --}
data Screen = Screen Horizontal Double
  deriving (Eq, Show)

cartesian :: Horizontal -> Vector3
cartesian (Horizontal az al) = Vector3
  { v3x = sin incl * cos (angle az)
  , v3y = sin incl * sin (angle az)
  , v3z = cos incl
  }
  where
    incl = (pi / 2) - angle al

screenCoord :: Screen -> Horizontal -> Maybe P.Point
screenCoord s (Horizontal (Azimuth az) (Altitude h))
  | h > 0 =
      let v = screenIntersect s (Horizontal (Azimuth az) (Altitude h))
      in case v of
        Just p  -> Just $ relativeCoord s p
        Nothing -> Nothing
  | otherwise = Nothing

relativeCoord :: Screen -> Vector3 -> P.Point
relativeCoord s w = solveLinearEq (grid s) v
  where
    v = w - origin s

solveLinearEq :: (Vector3, Vector3) -> Vector3 -> P.Point
solveLinearEq (v, w) a = (double2Float p, double2Float q)
  where
    pacc
      | abs (v3x v) >= abs (v3y v) && abs (v3x v) >= abs (v3z v) = v3x
      | abs (v3y v) >= abs (v3x v) && abs (v3y v) >= abs (v3z v) = v3y
      | otherwise = v3z

    qacc
      | abs (v3x w) >= abs (v3y w) && abs (v3x w) >= abs (v3z w) = v3x
      | abs (v3y w) >= abs (v3x w) && abs (v3y w) >= abs (v3z w) = v3y
      | otherwise = v3z

    q = (qacc a * pacc v - pacc a * qacc v) / (qacc w * pacc v + pacc w * qacc v)
    p = (pacc a - q * pacc w) / pacc v

origin :: Screen -> Vector3
origin (Screen vdir dist) =
  let snv = cartesian vdir
  in vnormalise snv |* dist

normalVector :: Screen -> Vector3
normalVector (Screen vdir _) = cartesian vdir

grid :: Screen -> (Vector3, Vector3)
grid (Screen (Horizontal az al) _) = (r x, r y)
  where
    x = Vector3 0 1 0
    y = Vector3 0 0 1
    r1 = rotateT AxisX AxisZ (Radians (angle al))
    r2 = rotateT AxisX AxisY (Radians (angle az))
    r v = transformP3 r2 (transformP3 r1 v)

screenIntersect :: Screen -> Horizontal -> Maybe Vector3
screenIntersect s hor = if ln /= 0 && f > 0
                          then Just $ f *| lv
                          else Nothing
  where
    lv = cartesian hor
    ln = lv `vdot` normalVector s
    f = (origin s `vdot` normalVector s) / ln
    
    
