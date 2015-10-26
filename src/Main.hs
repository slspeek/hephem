module Main where

import HEphem.HEphem
import           Data.Time.Clock
import Graphics.Gloss.Interface.IO.Game
import GHC.Float

north :: Screen
north = Screen (Horizontal (Azimuth 45) (Altitude 52)) 10

main:: IO ()
main = do 
  playIO (InWindow "HEphem" (1024,768) (10,10))
         blue
         5
         (World brightstarlist north)
         pictureWorld
         (\event  world -> return world)
         (\time world -> return world)
         
pictureWorld::World -> IO Picture
pictureWorld (World bs scr)  = 
  let pictureStar s t = case screenCoord scr  (snd (horizontal geoAms t s)) of Just (x,y) -> Translate (10*x) (10*y)  $ circleSolid (double2Float(6 - bMagitude s))
                                                                               Nothing    -> Blank   
  in
  do
    utc <- getCurrentTime
    return $ Pictures [
          pictureStar s utc | s <- brightstarlist]
    {-return $ pictures [ translate 0 100 $ circleSolid 5, translate (-50) (-100) $ circleSolid 3, circleSolid 10]-}


