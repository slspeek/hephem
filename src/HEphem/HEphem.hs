{-# LANGUAGE TemplateHaskell #-}
module HEphem.HEphem where

import           Text.ParserCombinators.ReadP
import Data.String
import Data.FileEmbed

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
         , bMagitude :: Float
         , bUminB :: Maybe Float
         , bBminV :: Float
         , bSpectralType :: String
         }
  deriving Show

star :: ReadP BrightStar
star = do
  name <- readName
  hr <- readHRNo
  ra <- readRA
  d <- readDec
  notes <- readNotes
  mag <- readMagnitude
  uminB <- readUminB
  bminV <- readBminV
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

nget :: Int -> ReadP String
nget n = count n get

readName :: ReadP String
readName = nget 19

readHRNo :: ReadP Int
readHRNo = do
  s <- nget 6
  return $ read s

readRA :: ReadP RA
readRA = do
  h <- nget 4
  let hi = read h
  m <- nget 3
  let mi = read m
  s <- nget 5
  let sf = read s
  return $ RA hi mi sf

replace:: String -> String
replace = map
            (\c -> if c == '+'
                     then ' '
                     else c)

readDec :: ReadP Dec
readDec = do
  h <- nget 6
  let h' = replace h
  let hi = read h'
  m <- nget 3
  let mi = read m
  s <- nget 3
  let sf = read s
  return $ Dec hi mi sf

readNotes :: ReadP String
readNotes = nget 9

readMagnitude :: ReadP Float
readMagnitude = do
  m <- nget 6
  return $ read m

readUminB :: ReadP (Maybe Float)
readUminB = do
  m <- nget 6
  if m == "      "
    then return Nothing
    else return $ Just (read (replace m))

readBminV :: ReadP Float
readBminV = do
  m <- nget 5
  return $ read (replace m)

readSpectralType :: ReadP String
readSpectralType = many1 get

main:: IO ()
main = do
  ls <- fmap (drop 5 . lines) (readFile "brightstar_2015/brightstar_2015.txt")
  let lshort = ls
  print $ map (fst . last . readP_to_S star) lshort

class HasAngle a where
  angle :: a -> Double

data Equatorial = Equatorial RA Dec
  deriving (Eq, Show)

data RA = RA Int Int Double
  deriving (Eq, Show)

instance HasAngle RA where
  angle (RA h m s) = (((fromIntegral h * 15.0) + ((fromIntegral m / 60.0) + (s / 3600.0))) / 180) * pi

data Dec = Dec Int Int Double
  deriving (Eq, Show)

instance HasAngle Dec where
  angle (Dec d m s) = ((fromIntegral d + ((fromIntegral m / 60.0) + (s / 3600.0))) / 180) * pi

data Horizontal = Horizontal Azimuth Ascent
  deriving (Eq, Show)

data Azimuth = Azimuth Double
  deriving (Eq, Show)

data Ascent = Ascent Double
  deriving (Eq, Show)

rigel :: Equatorial
rigel = Equatorial (RA 5 14 32.3) (Dec (-8) 12 5.9)

vega :: Equatorial
vega = Equatorial (RA 18 26 56.3) (Dec 38 47 1.9)

deneb :: Equatorial
deneb = Equatorial (RA 20 41 25.9) (Dec 45 16 49.5)

rA :: Equatorial -> Double
rA (Equatorial ra _) = angle ra

dec :: Equatorial -> Double
dec (Equatorial _ d) = angle d

