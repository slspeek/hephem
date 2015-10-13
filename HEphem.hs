module Main where

class HasAngle a where
		angle :: a -> Double
		

data Equatorial = Equatorial RA Dec deriving (Eq, Show)

data RA = RA Int Int Double deriving (Eq, Show)

instance HasAngle RA where
		angle  (RA h m s) = (((fromIntegral h * 15.0) + ((fromIntegral m /60.0) + (s/3600.0)))/180 ) * pi

data Dec = Dec Int Int Double deriving (Eq, Show)

instance HasAngle Dec where
		angle  (Dec d m s) = ((fromIntegral d + ((fromIntegral m /60.0) + (s/3600.0)))/180 ) * pi


data Horizontal = Horizontal Azimuth Ascent deriving (Eq, Show)

data Azimuth = Azimuth Double deriving (Eq, Show)

data Ascent = Ascent Double deriving (Eq, Show)

rigel :: Equatorial
rigel = Equatorial (RA 5 14 32.3) (Dec (-8) 12 5.9)

vega :: Equatorial
vega = Equatorial (RA 18 26 56.3) (Dec 38 47 1.9)

deneb :: Equatorial
deneb = Equatorial (RA 20 41 25.9) (Dec 45 16 49.5)

rA :: Equatorial -> Double
rA (Equatorial ra _) =  angle ra

dec :: Equatorial -> Double
dec (Equatorial _ d) = angle d

main :: IO ()
main = do 
				putStrLn "HEphem is free software."
				putStrLn $ "Rigel is at equatorial position " ++ show rigel
				putStrLn $ "Rigels RA is  " ++ show (  rA rigel ) ++ " and declination is " ++ show ( dec rigel) 
				putStrLn "Done. Thank you."
