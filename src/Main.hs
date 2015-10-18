module Main where

import HEphem.HEphem

main:: IO ()
main = print $ filter (\x -> bMagitude x < 1) brightstarlist

