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

geoAms :: GeoLoc
geoAms = GeoLoc (fromDMS 52 21 0) (fromDMS 4 51 59)

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
         , bRA :: Deg
         , bDec :: Deg
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

readRA :: ReadP Deg
readRA = do
  h <- getn 3
  let hi = read h
  m <- getn 3
  let mi = read m
  s <- getn 5
  let sf = read s
  return $ fromHMS hi mi sf

replace :: String -> String
replace = map
            (\c -> if c == '+'
                     then ' '
                     else c)

readDec :: ReadP Deg
readDec = do
  h <- getn 5
  let h' = replace h
  let hi = read h'
  m <- getn 3
  let mi = read m
  s <- getn 3
  let sf = read s
  return $ fromDMS hi mi sf

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

bEquatorial :: BrightStar -> EqPos
bEquatorial b = EqPos (bRA b) (bDec b)

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

type Deg = Degrees Double

instance (AEq a) => AEq (Degrees a) where
  (Degrees x) =~ (Degrees y) = x =~ y

instance (AEq a) => AEq (Radians a) where
  (Radians x) =~ (Radians y) = x =~ y

fromDMS :: Int -> Int -> Double -> Deg
fromDMS d m s = Degrees $ todec d m s

fromHMS :: Int -> Int -> Double -> Deg
fromHMS h m s = 15.0 * dhours
  where
    dhours = fromDMS h m s

todec :: (Fractional a, Integral a1, Integral a2) => a1 -> a2 -> a -> a
todec d m s = fromIntegral d + ((fromIntegral m / 60.0) + (s / 3600.0))

data GeoLoc = GeoLoc { gLatitude,gLongitude :: Deg }
  deriving (Eq, Show)

data EqPos = EqPos { eRA,eDec :: Deg }
  deriving (Eq, Show)

data HorPos = HorPos { hAzimuth,hAltitude :: Deg }
  deriving (Eq, Show)

instance Arbitrary Screen where
  arbitrary = liftM2 Screen arbitrary (suchThat arbitrary (> 1))

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

toMinutesSeconds :: Deg -> (Int, Int, Int)
toMinutesSeconds (Degrees d) = (i, m, s)
  where
    i = floor d
    r = d - fromIntegral i
    m = r `div'` (1 / 60)
    r' = r - fromIntegral m * (1 / 60)
    s = r' `div'` (1 / 3600)

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

{-- For graphical representation --}
data World = World { wStars :: [BrightStar], wScreen :: Screen }
  deriving (Eq, Show)

{-- Viewing screen has a direction and distance --}
data Screen = Screen HorPos Double
  deriving (Eq, Show)

cartesian :: HorPos -> Vector3
cartesian (HorPos az al) = Vector3
  { v3x = sine incl * cosine az
  , v3y = sine incl * sine az
  , v3z = cosine incl
  }
  where
    incl = Degrees 90 - al

screenCoord :: Screen -> HorPos -> Maybe P.Point
screenCoord s (HorPos az h)
  | h > 0 =
      let v = screenIntersect s (HorPos az h)
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
grid (Screen (HorPos az al) _) = (r x, r y)
  where
    x = Vector3 0 1 0
    y = Vector3 0 0 1
    r1 = rotateT AxisX AxisZ (radians al)
    r2 = rotateT AxisX AxisY (radians az)
    r v = transformP3 r2 (transformP3 r1 v)

screenIntersect :: Screen -> HorPos -> Maybe Vector3
screenIntersect s hor = if ln /= 0 && f > 0
                          then Just $ f *| lv
                          else Nothing
  where
    lv = cartesian hor
    ln = lv `vdot` normalVector s
    f = (origin s `vdot` normalVector s) / ln
    
