{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Ppm where

import Text.Printf
import Control.Monad.ST.Lazy
import Data.Array.ST

class Image a s where
    get :: a -> (Int, Int) -> ST s Pixel
    set :: a -> (Int, Int) ->  Pixel -> ST s ()
    width :: a -> Int
    height :: a -> Int

data PPMImage s = PPMImage {
     ppmArray :: STArray s (Int,Int) Pixel
    ,ppmWidth :: Int
    ,ppmHeight :: Int
    ,ppmName :: String
    }

data SubImage s = SubImage {
     parent :: PPMImage s
    ,offsetX :: Int
    ,offsetY :: Int
    ,subWidth :: Int
    ,subHeight :: Int
    }

instance Image (PPMImage s) s where
    get img (x,y) = do
      readArray (ppmArray img) (y,x) 
    set img (x,y) px = do
      writeArray (ppmArray img) (y,x) px
    width img  = ppmWidth img
    height img = ppmWidth img

instance Image (SubImage s) s where
    get (SubImage parent x' y' _ _) (x,y) = 
        let (rx, ry) = (x'+x , y'+y)
        in get parent (rx,ry)
    set (SubImage parent x' y' _ _) (x,y) =
        let (rx, ry) = (x'+x , y'+y)
        in set parent (rx,ry)
    width _  = 42
    height _ = 42
      
data Pixel = Pixel {
      pR :: Int
    , pG :: Int
    , pB :: Int
    } deriving Eq

-- contains information about an image that is useful for mosaic tile matching (only medianColor so far)
data Fingerprint = Fingerprint {
      fpFilename :: String
    , fpMedian :: Pixel
    } deriving (Show)

-- Shows contents of a pixel as string.
instance Show Pixel where
    show (Pixel r g b) = printf "%d %d %d" r g b