{-# LANGUAGE TemplateHaskell #-}

module HEphem.NGCParser where

import           Data.FileEmbed
import           Data.Maybe
import           Data.String
import           HEphem.Data
import           HEphem.ParserUtil
import           Text.ParserCombinators.ReadP
import           Text.Read                    (readMaybe)

readMaybeFloatWithComma :: String -> Maybe Float
readMaybeFloatWithComma =  readMaybe . sanitize
  where
    sanitize = map (\c -> if c == ',' then '.' else c)

ngcobjecttext :: IsString a => a
ngcobjecttext = $(embedStringFile "ngc-ic2000/ngc-ic2000.csv")

ngcObjectList :: [NGCObject]
ngcObjectList = mapMaybe readNGCObject (lines ngcobjecttext)

readRecord :: String -> [String]
readRecord = fst . last . readP_to_S p
  where
      p = sepBy (many (satisfy (/='|')))(char '|')

readNGCObject :: String -> Maybe NGCObject
readNGCObject st = do
  h <- readMaybe (l !! 5)
  m <- readMaybe (l !! 7)
  s <- readMaybeFloatWithComma (l !! 9)
  let ra = fromHMS h m s
  mag <- readMaybeFloatWithComma (l !! 18)
  d <- readMaybe (l !! 11)
  m' <- readMaybe (l !! 13)
  s' <- readMaybeFloatWithComma (l !! 15)
  let dec = fromDMS d m' s'
  return  NGCObject
    { nID = head l
    , nPGC= l !! 1
    , nMessier= l !! 2
    , nType= l !! 3
    , nClass= l !! 4
    , nRA= ra
    , nDec= dec
    ,nMag= mag
    }
  where
     l = readRecord st
