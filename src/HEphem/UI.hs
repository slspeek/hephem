{-# LANGUAGE TemplateHaskell #-}


module HEphem.UI where

import           Control.Arrow
import           Control.Lens                     hiding (element)
import           Control.Monad
import           Data.Angle
import           Data.Fixed                       (mod')
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Vector.Class
import           Data.Vector.Fancy
import           Data.Vector.Transform.Fancy
import           Data.Vector.Transform.T3
import           Data.Vector.V3
import           GHC.Float
import qualified Graphics.Gloss.Data.Point        as P
import           Graphics.Gloss.Interface.IO.Game
import           HEphem.Data
import           HEphem.HEphem
import           Test.QuickCheck
import           Text.Printf

instance Arbitrary Screen where
  arbitrary = liftM2 Screen arbitrary (suchThat arbitrary (> 1))

-- | World in the Gloss Game sense
data World = World
  { _wObjects              :: [SkyObject]
  , _wScreen               :: Screen
  , _wGeo                  :: GeoLoc
  , _wLlc                  :: (Int, Int)
  , _wCurrentTime          :: UTCTime
  , _wTimeFactor           :: Float
  , _wMinMag               :: Float
  , _wHighlightedSkyObject :: Maybe SkyObject
  }
  deriving (Show)

{-- Viewing screen has a direction and distance --}
data Screen = Screen
  { _sDirection :: HorPos
  , _sDistance  :: Double
  }
  deriving (Eq, Show)

makeLenses ''World
makeLenses ''Screen

origin :: Screen -> Vector3
origin (Screen vdir dist) =
  let snv = cartesian (vdir, 1)
  in vnormalise snv |* dist

normalVector :: Screen -> Vector3
normalVector (Screen vdir _) = cartesian (vdir, 1)

grid :: Screen -> (Vector3, Vector3)
grid (Screen (HorPos az al) _) = (r x, r y)
  where
    x = Vector3 0 1 0
    y = Vector3 0 0 1
    r1 = rotateT AxisX AxisZ (radians al)
    r2 = rotateT AxisX AxisY (radians az)
    r v = transformP3 r2 (transformP3 r1 v)

screenIntersect :: Screen -> HorPos -> Maybe Vector3
screenIntersect s hor = if abs ln > 0.01 && f > 0
                          then Just $ f *| lv
                          else Nothing
  where
    lv = cartesian (hor, 1)
    ln = lv `vdot` normalVector s
    f = (origin s `vdot` normalVector s) / ln


screenCoord :: Screen -> HorPos -> Maybe P.Point
screenCoord s (HorPos az h)
  | h > 0 =
      let v = screenIntersect s (HorPos az h)
      in case v of
        Just p  -> relativeCoord s p
        Nothing -> Nothing
  | otherwise = Nothing

screenCoordToHorPos :: Screen -> (Float, Float) -> HorPos
screenCoordToHorPos s (x, y) = fst (polair ( origin s + float2Double x *| v  +  float2Double y *| w))
  where
    (v, w) = grid s


relativeCoord :: Screen -> Vector3 -> Maybe P.Point
relativeCoord s w = fmap (double2Float *** double2Float) (solveLinearEq (grid s) v)
  where
    v = w - origin s

solveLinearEq :: (Vector3, Vector3) -> Vector3 -> Maybe (Scalar, Scalar)
solveLinearEq (v, w) a = if null dlist then
                            Nothing
                         else Just closest
  where
    closest = snd . Map.findMin $ Map.fromList dlist
    dlist = [ (dis z, z) | z <- nums]
    dis (x, y) = vmag $ (x *| v + y *| w) - a
    nums = [(x, y) | (x, y) <- l
                   , isNum x
                   , isNum y]
    l = [useToSolve accA accB (v, w) a | accA <- [v3x, v3y, v3z]
                                       , accB <- [v3x, v3y, v3z]
                                       , accA dum /= accB dum]
    dum = Vector3 0 1 2
    isNum x = not (isNaN x) && not (isInfinite x)


useToSolve :: Fractional t1 => (t -> t1) -> (t -> t1) -> (t, t) -> t -> (t1, t1)
useToSolve accA accB (v, w) a = (p, q)
  where
    q = (accB a * accA v - accA a * accB v) / (accB w * accA v - accA w * accB v)
    p = (accA a - q * accA w) / accA v


pictureSkyObject:: (SkyObject, (Float, Float))-> Picture
pictureSkyObject (NGC so, pos)  = pictureNGCObject so pos
pictureSkyObject (Star so, pos) = pictureStar so pos


pictureStar:: BrightStar -> (Float,Float) -> Picture
pictureStar s (x, y) = Color white . Translate x y $ circleSolid (max 1 (6 - bMag s))

pictureNGCObject:: NGCObject -> (Float,Float) -> Picture
pictureNGCObject n (x, y) = Pictures
  [ Color blue . Translate x y $ circle (max 1 (6 - nMag n))
  , Color blue . Translate (x + 10) (y - 10) . Scale 0.1 0.1 $ text (nMessier n)
  ]


iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%F %T"

fromDeg :: Deg -> Double
fromDeg (Degrees x) = x

pictureDashboard:: World -> Picture
pictureDashboard w = Color red . Translate (fromIntegral x + 10) (fromIntegral y + 8) . Scale 0.1 0.1 .
  Text $ printf "Azimuth: %.2f  Altitude: %.2f  Distance: %.2f Time: %s Speed: %.2f  Minimal mag: %.0f"
                (fromDeg (w^.wScreen.sDirection.hAzimuth))
                (fromDeg (w^.wScreen.sDirection.hAltitude))
                (w^.wScreen.sDistance)
                (iso8601  (w^.wCurrentTime))
                (w^.wTimeFactor)
                (w^.wMinMag)
  where
    (x, y) = w^.wLlc

screenCoordAt:: Screen -> GeoLoc -> UTCTime ->SkyObject-> Maybe(Float,Float)
screenCoordAt scr geo t so = screenCoord scr (equatorialToHorizontal geo t (equatorial so))

onScreenObjects:: World -> UTCTime -> [(SkyObject, (Float,Float))]
onScreenObjects w t = mapMaybe f $ visibleObjects w
  where
    f so = do
      c <- screenCoordAt (w^.wScreen) (w^.wGeo) t so
      return (so, c)

visibleObjects:: World -> [SkyObject]
visibleObjects w = filter (\x -> magnitude x < w^.wMinMag) (w^.wObjects)

pictureHighlightedObject ::World ->  SkyObject -> Picture
pictureHighlightedObject w s = case mc of Just (x,y) -> pict x y
                                          _          -> Blank
  where
      mc = screenCoordAt (w^.wScreen) (w^.wGeo) (w^.wCurrentTime) s
      pict x y = Pictures
        [ Color green . Translate x y $ circle (max 1 (6 - magnitude s))
        , Color green . Translate (x + 10) (y - 10) . Scale 0.1 0.1 $ text (description s)
        ]


pictureWorld :: World -> Picture
pictureWorld w =
  let stars = Pictures $ fmap pictureSkyObject (onScreenObjects w (w^.wCurrentTime));
      highlight = maybe Blank (pictureHighlightedObject w) (w ^. wHighlightedSkyObject)
  in Pictures [stars, pictureDashboard w, highlight]


starColor :: Color
starColor = white

advanceTime:: Float -> World -> World
advanceTime t w = over wCurrentTime (addUTCTime (realToFrac (w^.wTimeFactor * t))) w

dmod :: Deg -> Int -> Deg
dmod (Degrees d) i = Degrees (d `mod'` fromIntegral i)

findSkyObject:: World -> (Float,Float) -> Maybe SkyObject
findSkyObject w p = findNear (visibleObjects w) eq 0.01
  where
    hor = screenCoordToHorPos (w^.wScreen) p
    eq = horizontalToEquatorial (w^.wGeo) (w^.wCurrentTime) hor

eventHandler :: Event -> World -> World
eventHandler ev w =
  case ev of
    EventKey (SpecialKey KeyLeft) Up _ _  -> over (wScreen . sDirection . hAzimuth) (\x -> (x-3) `dmod` 360) w
    EventKey (SpecialKey KeyRight) Up _ _ -> over (wScreen . sDirection . hAzimuth) (\x -> (x+3) `dmod` 360) w
    EventKey (SpecialKey KeyUp) Up _ _    -> over (wScreen . sDirection . hAltitude) (+3) w
    EventKey (SpecialKey KeyDown) Up _ _  -> over (wScreen . sDirection . hAltitude) (-3+) w
    EventKey (Char 'w') Up _ _            -> over (wScreen . sDistance) (* 1.1) w
    EventKey (Char 's') Up _ _            -> over (wScreen . sDistance) (/ 1.1) w
    EventKey (Char 'e') Up _ _            -> over wTimeFactor (* 2) w
    EventKey (Char 'd') Up _ _            -> over wTimeFactor (/ 2) w
    EventKey (Char 'r') Up _ _            -> over wMinMag (+1) w
    EventKey (Char 'f') Up _ _            -> over wMinMag (+ (-1)) w
    EventKey (MouseButton LeftButton) Up _ (x,y) ->  set wHighlightedSkyObject (findSkyObject w (x,y)) w
    EventResize (x, y) ->  set wLlc (- x `div` 2, - y `div` 2) w
    _ ->   w
