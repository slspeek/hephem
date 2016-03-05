module HEphem.ParserUtil where

import           Data.Angle
import           GHC.Float
import           HEphem.Data

toDecimal:: (Integral a, Integral a1) => a -> a1 -> Float -> Double
toDecimal d m s = fromIntegral d + ((fromIntegral m / 60.0) + (float2Double s / 3600.0))

fromDMS :: Int -> Int -> Float -> Deg
fromDMS d m s = Degrees $ toDecimal d m s

fromHMS :: Int -> Int -> Float -> Deg
fromHMS h m s = 15.0 * dhours
  where
    dhours = fromDMS h m s
