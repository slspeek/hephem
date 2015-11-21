{-# LANGUAGE TemplateHaskell #-}

module HEphem.BSParser (
    Deg,
    fromDMS,
    fromHMS,
    brightstarlist,
    EqPos(EqPos),
    eRA,
    eDec,
    BrightStar,
    bHRNo,
    bNotes,
    bMagitude,
    bName,
    bEquatorial,
    bSpectralType,
    bBminV,
    bUminB,
    toMinutesSeconds,
    starReadP,
    readDec,
    ) where

import           Text.ParserCombinators.ReadP
import           Data.String
import           Data.FileEmbed
import           Data.Angle
import           Data.Fixed (div')

type Deg = Degrees Double

data EqPos = EqPos { eRA,eDec :: Deg }
  deriving (Eq, Show)

toDecimal :: (Fractional a, Integral a1, Integral a2) => a1 -> a2 -> a -> a
toDecimal d m s = fromIntegral d + ((fromIntegral m / 60.0) + (s / 3600.0))

fromDMS :: Int -> Int -> Double -> Deg
fromDMS d m s = Degrees $ toDecimal d m s

fromHMS :: Int -> Int -> Double -> Deg
fromHMS h m s = 15.0 * dhours
  where
    dhours = fromDMS h m s

brightstartext :: IsString a => a
brightstartext = $(embedStringFile "brightstar_2015/brightstar_2015.txt")

brightstarlist :: [BrightStar]
brightstarlist =
  let starlines = (drop 5 . lines) brightstartext
  in map (fst . last . readP_to_S starReadP) starlines

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

starReadP :: ReadP BrightStar
starReadP = do
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

toMinutesSeconds :: Deg -> (Int, Int, Int)
toMinutesSeconds (Degrees d) = (i, m, s)
  where
    i = floor d
    r = d - fromIntegral i
    m = r `div'` (1 / 60)
    r' = r - fromIntegral m * (1 / 60)
    s = r' `div'` (1 / 3600)
