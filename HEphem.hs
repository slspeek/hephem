module Main where

{-module BrightStar where -}

import Text.ParserCombinators.ReadP
import Data.Char

{-Bright Star List for Epoch =2015.5-}
{-------------------------------------------------------------------------------------------------------}
{-Flamsteed/Bayer     |BS=HR | RA       |  Dec      |Notes   | V  |  U-B | B-V |Spectral Type-}
{-Designation         |No.   |          |           |        |    |      |     |-}
{-------------------------------------------------------------------------------------------------------}
{----}
perseus = "33   alpha    Per  1017   3 25 26.2   +49 54 54   das     1.79 +0.37 +0.48 F5 Ib"

decl ="   +49 54 54"  


data BrightStar = BrightStar {
										bName::String,
										bHRNo::Int,
										bRA::RA,
										bDec::Dec,
										bNotes::String,
										bMagitude::Float,
										bUminB::Maybe Float,
										bBminV::Float,
										bSpectralType::String
									} deriving Show

star :: ReadP BrightStar
star = do	
				name <- readName
				hr <- readHRNo
				ra <- readRA
				dec <- readDec
				notes <- readNotes
				mag <- readMagnitude
				uminB <- readUminB
				bminV <- readBminV
				spec <- readSpectralType

				return BrightStar {
							bName = name,
						  bHRNo = hr,
							bRA = ra,
							bDec = dec,
							bNotes = notes,
							bMagitude = mag,
							bUminB = uminB,
							bBminV = bminV,
							bSpectralType = spec 
						}	

readName :: ReadP String
readName = count 19 get
	
readHRNo :: ReadP Int
readHRNo = do 
						s <- count 6 get
						return $  read s

readRA :: ReadP RA
readRA = do 
					h <- count 4 get
					let hi 	= read h
					m <- count 3 get
					let mi = read m
					s <- count 5 get 
					let sf = read s
					return $ RA hi mi sf


replace = map (\c -> if c=='+' then ' '; else c)

readDec :: ReadP Dec
readDec = do 
					h <- count 6 get
					let h' = replace h
					let hi 	= read h'
					m <- count 3 get
					let mi = read m
					s <- count 3 get 
					let sf = read s
					return $ Dec hi mi sf

readNotes :: ReadP String
readNotes =  count 9 get

readMagnitude :: ReadP Float
readMagnitude = do 
								m <- count 6 get
								return $ read m

readUminB :: ReadP (Maybe Float)
readUminB = do 
								m <- count 6 get
								if m == "      " then
										return Nothing
								else
										return $ Just ( read (replace m))

readBminV :: ReadP Float
readBminV = do 
								m <- count 5 get
								return $ read (replace m)

readSpectralType :: ReadP String
readSpectralType = many1 get

main = print (readP_to_S star perseus)

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

{-main :: IO ()-}
{-main = do -}
				{-putStrLn "HEphem is free software."-}
				{-putStrLn $ "Rigel is at equatorial position " ++ show rigel-}
				{-putStrLn $ "Rigels RA is  " ++ show (  rA rigel ) ++ " and declination is " ++ show ( dec rigel) -}
				{-putStrLn "Done. Thank you."-}
