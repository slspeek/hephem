{-# LANGUAGE TemplateHaskell #-}

module HEphem.NGCParser where

import           Data.FileEmbed
import           Data.String
import           Text.ParserCombinators.ReadP

readFloatWithComma :: String -> Float
readFloatWithComma = read . sanitize
  where
    sanitize = map (\c -> if c == ',' then '.' else c)

ngcobjecttext :: IsString a => a
ngcobjecttext = $(embedStringFile "ngc-ic2000/ngc-ic2000.csv")

testReader :: String -> [String]
testReader = fst .last . readP_to_S (sepBy (many (satisfy (/='|')))(char '|'))
