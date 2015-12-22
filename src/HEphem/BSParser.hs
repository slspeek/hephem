{-# LANGUAGE TemplateHaskell #-}

module HEphem.BSParser (
    Deg,
    fromDMS,
    fromHMS,
    brightstarlist,
    starReadP,
    readDec,
    ) where

import           Data.FileEmbed
import           Data.String
import           HEphem.Data
import           HEphem.ParserUtil
import           Text.ParserCombinators.ReadP

brightstartext :: IsString a => a
brightstartext = $(embedStringFile "brightstar_2015/brightstar_2015.txt")

brightstarlist :: [BrightStar]
brightstarlist =
  let starlines = (drop 5 . lines) brightstartext
  in map (fst . last . readP_to_S starReadP) starlines

starReadP :: ReadP BrightStar
starReadP = do
  f <- readFlamsteed
  b <- readBayer
  c <- readConst
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

  return BrightStar
      { bFlamsteed = f
      , bBayer = b
      , bConst = c
      , bHRNo = hr
      , bRA = ra
      , bDec = d
      , bNotes = notes
      , bMag = mag
      , bUminB = uminB
      , bBminV = bminV
      , bSpectralType = spec
      }

  where
    space = char ' '

trim:: String -> String
trim s = reverse . dropWhile (==' ') . reverse $ dropWhile (==' ') s

getn :: Int -> ReadP String
getn n = count n get

readFlamsteed :: ReadP (Maybe Int)
readFlamsteed = do
  s <- getn 4
  let f = trim s
  if f == ""
    then return Nothing
    else return . Just $ read s

readConst :: ReadP String
readConst = do
  s <- getn 3
  return $ trim s

readBayer :: ReadP String
readBayer = do
  s <- getn 12
  return $ trim s

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

readMagnitude :: ReadP Float
readMagnitude = do
  m <- getn 5
  return $ read m

readUminB :: ReadP (Maybe Float)
readUminB = do
  m <- getn 5
  if m == "     "
    then return Nothing
    else return . Just . read $ replace m

readBminV :: ReadP Float
readBminV = do
  m <- getn 5
  return . read $ replace m

readSpectralType :: ReadP String
readSpectralType = many1 get
