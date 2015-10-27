module Main where

import HEphem.HEphem
import           Data.Time.Clock
import Graphics.Gloss.Interface.IO.Game
import GHC.Float

north :: Screen
north = Screen (Horizontal (Azimuth 0) (Altitude 45)) 100

main:: IO ()
main = do 
  playIO (InWindow "HEphem" (1024,768) (10,10))
         blue
         5
         (World brightstarlist north)
         pictureWorld
         eventHandler
         (\time world -> return world)
         
pictureWorld::World -> IO Picture
pictureWorld (World bs scr)  = 
  let pictureStar s t = case screenCoord scr  (snd (horizontal geoAms t s)) of Just (x,y) -> Translate (10*x) (10*y)  $ circleSolid (double2Float(6 - bMagitude s))
                                                                               Nothing    -> Blank   
  in
  do
    utc <- getCurrentTime
    let stars =Pictures [ pictureStar s utc | s <- brightstarlist]
    let (Screen (Horizontal (Azimuth az)(Altitude h)) d) = scr
    let dashboard = Color red $ Translate (-500) (-360) $ Scale 0.1 0.1 $ Text $ "A: " ++ show az ++ " h: "++ show h ++ " d: " ++show d

    return $ Pictures [stars, dashboard]    {-return $ pictures [ translate 0 100 $ circleSolid 5, translate (-50) (-100) $ circleSolid 3, circleSolid 10]-}

eventHandler:: Event -> World -> IO World
eventHandler ev (World bs (Screen (Horizontal (Azimuth az) (Altitude h)) d) ) = 
  case ev of EventKey (SpecialKey KeyLeft)  Up _ _ -> return $ World bs (Screen (Horizontal (Azimuth (-3+az))(Altitude h)) d) 
             EventKey (SpecialKey KeyRight)  Up _ _ -> return $ World bs (Screen (Horizontal (Azimuth (3+az))(Altitude h)) d) 
             EventKey (SpecialKey KeyUp)  Up _ _ -> return $ World bs (Screen (Horizontal (Azimuth az)(Altitude (h+7))) d) 
             EventKey (SpecialKey KeyDown)  Up _ _ -> return $ World bs (Screen (Horizontal (Azimuth az)(Altitude (h-7))) d) 
             EventKey (Char 'w')  Up _ _ -> return $ World bs (Screen (Horizontal (Azimuth az)(Altitude h)) (d + 1)) 
             EventKey (Char 's')  Up _ _ -> return $ World bs (Screen (Horizontal (Azimuth az)(Altitude h)) (d - 1)) 
             _ -> return $  (World  bs (Screen (Horizontal (Azimuth az) (Altitude h)) d) )
