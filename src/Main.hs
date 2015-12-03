module Main where

import           Data.Angle
import           Graphics.Gloss.Interface.IO.Game
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
  playIO
    (InWindow "HEphem" (1024, 768) (10, 10))
    black
    5
    (World (brightstarlist ++ filter (\x -> nMessier x /="" || nMag x < 20) ngcObjectList) north geoAms (-512, -384))
    pictureWorld
    eventHandler
    (\_ world -> return world)
