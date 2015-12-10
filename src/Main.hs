module Main where

import           Data.Angle
import           Data.Time.Clock
import           Graphics.Gloss.Interface.Pure.Game
import           HEphem.BSParser
import           HEphem.Data
import           HEphem.HEphem
import           HEphem.NGCParser
import           HEphem.UI

north :: Screen
north = Screen (HorPos (Degrees 0) (Degrees 52)) 100

main :: IO ()
main = do
  putStrLn $ "HEphem  Copyright (C) 2015  Steven L. Speek\n" ++
             "This program comes with ABSOLUTELY NO WARRANTY\n" ++
             "This is free software, and you are welcome to redistribute it\n" ++
             "under certain conditions"
  utc <- getCurrentTime
  play
    (InWindow "HEphem" (1024, 768) (10, 10))
    black
    5
    (World (brightstarlist ++ ngcObjectList) north geoAms (-512, -384) utc 1 1)
    pictureWorld
    eventHandler
    advanceTime
