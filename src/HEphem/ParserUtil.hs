module HEphem.ParserUtil where

import           Data.Angle
import           Data.Fixed  (div')
import           HEphem.Data

toDecimal :: (Fractional a, Integral a1, Integral a2) => a1 -> a2 -> a -> a
toDecimal d m s = fromIntegral d + ((fromIntegral m / 60.0) + (s / 3600.0))

fromDMS :: Int -> Int -> Double -> Deg
fromDMS d m s = Degrees $ toDecimal d m s

fromHMS :: Int -> Int -> Double -> Deg
fromHMS h m s = 15.0 * dhours
  where
    dhours = fromDMS h m s

toMinutesSeconds :: Deg -> (Int, Int, Int)
toMinutesSeconds (Degrees d) = (i, m, s)
  where
    i = floor d
    r = d - fromIntegral i
    m = r `div'` (1 / 60)
    r' = r - fromIntegral m * (1 / 60)
    s = r' `div'` (1 / 3600)
