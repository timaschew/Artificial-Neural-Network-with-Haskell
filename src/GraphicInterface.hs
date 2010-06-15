
module GraphicInterface where

import Ppm

{--
example path: "data/traindata/img/10_12/big_numbers/0.pgm"

readPPMFile "data/traindata/img/10_12/big_numbers/0.pgm"
--}

readPPMFile :: String -> IO [Double]
readPPMFile fn = do
	inp <- readFile fn
	let lineList = lines inp
	let res = words (lineList !! 1)
	let width = read (res !! 0) :: Int
	let height = read (res !! 1) :: Int
	let pixels = drop 4 (words inp)
	return (map normalize pixels)
	
normalize x = norm where
	z = read x :: Double
	norm = z / 255