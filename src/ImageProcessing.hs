{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module ImageProcessing where

import ForSyDe.Shallow
import Prelude hiding (mapM)
import Data.List (intercalate, group)


type Pixel = (Integer, Integer, Integer)
type Image a = Vector (Vector a)

asciiLevels :: [Char]
asciiLevels = [' ','_',',','=',';','/','t','S','E','V','A','#','B','Q','N','@']

------------------------------------------------------------
-- vector utilities

reduceV f (x:>xs) = foldrV f x xs
mapMat     = mapV . mapV
zipWithMat = zipWithV . zipWithV

------------------------------------------------------------
-- vector main functions

grayscale :: Image Pixel -> Image Double
grayscale = mapMat convert
  where
    convert (r,g,b) = fromInteger r * 0.3125
                    + fromInteger g * 0.5625
                    + fromInteger b * 0.125

resize :: Image Double -> Image Double
resize = mapMat (/ 4) . sumRows . sumCols
  where
    sumCols = mapV (mapV (reduceV (+)) . groupV 2)
    sumRows = mapV (reduceV (zipWithV (+))) . groupV 2

sobel :: Image Double -> Image Double
sobel img = dropMargins $ zipWithMat (\x y -> sqrt (x * x + y * y) / 4) gx gy
  where
    dropMargins = mapV (tailV . initV) . tailV . initV
    gx = reduceV (zipWithMat (+)) $ vector [gx11, gx13, gx21, gx23, gx31, gx33]
    gy = reduceV (zipWithMat (+)) $ vector [gy11, gy12, gy13, gy31, gy32, gy33]
    -----------------------------------------------
    gx11 = mapMat (* (-1)) $ rotrV $ mapV rotrV img
--  gx12 = mapMat (*   0 ) $ rotrV $            img
    gx13 = mapMat (*   1 ) $ rotrV $ mapV rotlV img
    gx21 = mapMat (* (-2))         $ mapV rotrV img
--  gx22 = mapMat (*   0 )         $            img
    gx23 = mapMat (*   2 )         $ mapV rotlV img
    gx31 = mapMat (* (-1)) $ rotlV $ mapV rotrV img
--  gx32 = mapMat (*   0 ) $ rotlV $            img
    gx33 = mapMat (*   1 ) $ rotlV $ mapV rotlV img
    -----------------------------------------------
    gy11 = mapMat (* (-1)) $ rotrV $ mapV rotrV img
    gy12 = mapMat (* (-2)) $ rotrV $            img
    gy13 = mapMat (* (-1)) $ rotrV $ mapV rotlV img
--  gy21 = mapMat (*   0 )         $ mapV rotrV img
--  gy22 = mapMat (*   0 )         $            img
--  gy23 = mapMat (*   0 )         $ mapV rotlV img
    gy31 = mapMat (*   1 ) $ rotlV $ mapV rotrV img
    gy32 = mapMat (*   2 ) $ rotlV $            img
    gy33 = mapMat (*   1 ) $ rotlV $ mapV rotlV img

toAsciiArt :: Image Double -> Image Char
toAsciiArt = mapMat num2char
  where
    num2char n = asciiLevels !! level n
    level n = truncate $ nLevels * (n / 255)
    nLevels = fromIntegral $ length asciiLevels - 1

------------------------------------------------------------
-- SDF utilities

toImage :: Int -> Int -> [a] -> Image a
toImage x y list
  | lengthV image /= y = error "image dimention Y mismatch"
  | otherwise = image
  where
    image = part list
    part [] = NullV
    part l | length l < x = error "not enough pixels to fill last row"
           | otherwise    = (vector $ take x l) :> part (drop x l)

fromImage :: Image a -> [a]
fromImage =  concat  . map fromVector . fromVector

------------------------------------------------------------
-- SDF process network

dimX = 32
dimY = 32

imageProcessing :: Signal Pixel -> Signal Char
imageProcessing = asciiSDF . sobelSDF . resizeSDF . graySDF
  where
    graySDF   = mapSDF (x1 * y1) (x1 * y1) (fromImage . grayscale  . toImage x1 y1)
    resizeSDF = mapSDF (x1 * y1) (x2 * y2) (fromImage . resize     . toImage x1 y1)
    sobelSDF  = mapSDF (x2 * y2) (x3 * y3) (fromImage . sobel      . toImage x2 y2)
    asciiSDF  = mapSDF (x3 * y3) (x3 * y3) (fromImage . toAsciiArt . toImage x3 y3)
    x1 = dimX
    y1 = dimY
    x2 = dimX `div` 2
    y2 = dimY `div` 2
    x3 = (dimX `div` 2) - 2
    y3 = (dimY `div` 2) - 2

------------------------------------------------------------
-- Test functions

sourceSDF :: [Integer] -> Signal Pixel
sourceSDF = signal . map toPixel . splitEvery 3
  where toPixel [a,b,c] = (a,b,c)
        splitEvery _ [] = []
        splitEvery n list = first : (splitEvery n rest)
          where
            (first,rest) = splitAt n list

-- observeSDF :: Show a => Signal a -> IO ()
-- ovserveSDF = do
--   let 

-- dimX = 14; dimY = 10
testImage :: [Integer]
testImage = [
  0,0,0,0,0,0,255,0,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,0,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,0,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,0,255,0,0,0,0,0,0]

testImage2 :: [Integer]
testImage2 = [
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

testImage3 :: Image Pixel
testImage3 = vector [
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [m,m,m,m,m,m,m,m,m,m,m,m,m,m],
  vector [m,m,m,m,m,m,m,m,m,m,m,m,m,m],
  vector [m,m,m,m,m,m,m,m,m,m,m,m,m,m],
  vector [m,m,m,m,m,m,m,m,m,m,m,m,m,m],
  vector [m,m,m,m,m,m,m,m,m,m,m,m,m,m],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o]]
  where m = (255, 255, 255)
        o = (0, 0, 0)

testImage4 :: Image Pixel
testImage4 = vector [
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,m,o,o,o,o,o,o,o],
  vector [o,o,o,o,o,o,o,o,o,o,o,o,o,o]]
  where m = (255, 255, 255)
        o = (0, 0, 0)       
        

showImage :: Show a => Image a -> IO ()
showImage = putStrLn . intercalate "\n" . map show . fromVector
