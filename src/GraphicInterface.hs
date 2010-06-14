
module GraphicInterface where

import Text.Printf
import System.Environment
import System.Directory
import Data.List
import Data.Char
import Control.Parallel
import Control.Monad
import Control.Monad.ST.Lazy
import Data.Array.ST
import qualified Data.ByteString.Lazy.Char8 as BS

import Ppm

-- let fn = "data/traindata/img/10_12/big_numbers/0.pgm"

{--
readPPMFile fn = do
	rawImage <- BS.readFile fn
	image <- readPPM (rawImage, fn)
	return image
--}

importPPM fn = do
	inp <- readFile fn
	let lineList = lines inp
	let res = (lines inp) !! 1
	let width = read (head res) :: Int
	let height = read (tail res) :: Int
	let pixels = drop 4 (words inp)
	let result = map (normalize) pixels
	return result
	
normalize x = norm where
	z = read x :: Double
	z / 255

-- Takes a filename and opens it as a ppm image.
readPPM :: (BS.ByteString, String) -> ST s (PPMImage s)
readPPM (rawImage, fn) = do
    let content = BS.lines rawImage
        [w,h] = map (read . BS.unpack) $ BS.words (content !! 1)
        pixel = readInts $ BS.unwords $ drop 3 content
    xy <- newListArray ((1,1), (h,w)) (str2pix pixel) :: ST s (STArray s (Int,Int) Pixel)
    return PPMImage {
           ppmWidth = w
         , ppmHeight = h
         , ppmName = fn
         , ppmArray = xy }
  where readInts s =
            case BS.readInt s of
                Nothing -> []
                Just (v,rest) -> v : readInts (next rest)
        next s = BS.dropWhile isSpace s
        
        

 -- Takes a list of strings (representing lines of a ppm file) and turns them
-- into a list of pixels.
str2pix :: [Int] -> [Pixel]
str2pix [] = []
str2pix (r:g:b:xs) = Pixel r g b : (str2pix xs)