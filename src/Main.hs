module Main where

import           Data.Angle
import           Data.Time.Clock
import           Graphics.Gloss.Interface.IO.Game
import           HEphem.BSParser
import           HEphem.Data
import           HEphem.HEphem
import           HEphem.NGCParser
import           HEphem.UI

north :: Screen
north = Screen (HorPos (Degrees 45) (Degrees 90)) 100

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
    (World (brightstarlist ++ filter (\x -> nMessier x /="" || nMag x < 7) ngcObjectList) north)
    pictureWorld
    eventHandler
    (\_ world -> return world)

pictureWorld :: World -> IO Picture
pictureWorld (World bs scr) =
  let pictureStar s t =
                         case screenCoord scr (snd (horizontal geoAms t s)) of
                           Just (x, y) -> case s of BrightStar{} ->
                                                      Color white . Translate (10 * x) (10 * y) $ circleSolid (6 - magnitude s)
                                                    _ ->
                                                      Pictures [Color blue . Translate (10 * x) (10 * y) $ circle (6 - magnitude s),
                                                              Color blue . Translate (10 * x + 10) (10 * y - 10) $ Scale 0.1 0.1 $ text (nMessier s)]
                           Nothing -> Blank;


  in do
    utc <- getCurrentTime
    let stars = Pictures [pictureStar s utc | s <- bs]
    let (Screen (HorPos az h) d) = scr
    let dashboard = Color red $ Translate (-500) (-360) $ Scale 0.1 0.1 $ Text $ "A: " ++
                                                                                 show az ++
                                                                                 " h: " ++
                                                                                 show h ++
                                                                                 " d: " ++
                                                                                 show d
    return $ Pictures [stars, dashboard]

starColor :: Color
starColor = white

eventHandler :: Event -> World -> IO World
eventHandler ev (World bs (Screen (HorPos az h) d)) =
  case ev of
    EventKey (SpecialKey KeyLeft) Up _ _  -> return $ World bs (Screen (HorPos (-3 + az) h) d)
    EventKey (SpecialKey KeyRight) Up _ _ -> return $ World bs (Screen (HorPos (3 + az) h) d)
    EventKey (SpecialKey KeyUp) Up _ _    -> return $ World bs (Screen (HorPos az (h + 3)) d)
    EventKey (SpecialKey KeyDown) Up _ _  -> return $ World bs (Screen (HorPos az (h - 3)) d)
    EventKey (Char 'w') Up _ _            -> return $ World bs (Screen (HorPos az h) (d * 1.1))
    EventKey (Char 's') Up _ _            -> return $ World bs (Screen (HorPos az h) (d / 1.1))
    _ -> return $  World  bs (Screen (HorPos az  h) d)
