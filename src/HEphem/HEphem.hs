{-# LANGUAGE TemplateHaskell #-}

module HEphem.HEphem where

import           Text.ParserCombinators.ReadP
import           Data.String
import           Data.FileEmbed
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Fixed (mod', div')

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
  deriving Show

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

class HasAngle a where
  angle :: a -> Double

torad :: Int -> Int -> Double -> Double
torad d m s = (todeg d m s / 180) * pi

todeg :: Int -> Int ->Double -> Double
todeg d m s = fromIntegral d + ((fromIntegral m / 60.0) + (s / 3600.0))

data GeoLocation = GeoLocation Latitude Longitude
  deriving (Eq, Show)

data Latitude = Latitude Int Int Double
  deriving (Eq, Show)

data Longitude = Longitude Int Int Double
  deriving (Eq, Show)



data Equatorial = Equatorial RA Dec
  deriving (Eq, Show)

data RA = RA Int Int Double
  deriving (Eq, Show)

instance HasAngle RA where
  angle (RA h m s) = torad (15 * h) m s

data Dec = Dec Int Int Double
  deriving (Eq, Show)

instance HasAngle Dec where
  angle (Dec d m s) = torad d m s

data Horizontal = Horizontal Azimuth Ascent
  deriving (Eq, Show)

data Azimuth = Azimuth Double
  deriving (Eq, Show)

data Ascent = Ascent Double
  deriving (Eq, Show)

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
    {-- need the 0.5 to get from modified julian date to reduced julian date --}
    d = fracDays utc - fromIntegral time20000101 - 0.5;
    time20000101 = toModifiedJulianDay $ fromGregorian 2000 1 1

currentSiderealtime :: IO Double
currentSiderealtime = siderealtime <$> getCurrentTime

currentLocalSiderealtime :: Longitude -> IO Double
currentLocalSiderealtime l = localSiderealtime l<$> getCurrentTime

toMinutesSeconds :: Double -> (Int, Int, Int)
toMinutesSeconds d = (i, m, s)
  where
    i = floor d;
    r = d - fromIntegral i;
    m = r `div'` (1/60);
    r' = r - fromIntegral m * (1/60);
    s = r' `div'` (1/3600) ;

localSiderealtime :: Longitude -> UTCTime -> Double
localSiderealtime (Longitude d m s)  utc = (siderealtime utc + todeg d m s  / 15) `mod'` 24


