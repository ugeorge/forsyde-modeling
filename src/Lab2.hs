module Lab2 where

import ForSyDe.Shallow
import Control.Monad.Trans.State

wrongDimensionsError :: a
wrongDimensionsError = error "Wrong dimensions!"

grayscale :: [Int] -> [Int]
grayscale [r,g,b] = [truncate rgray + truncate ggray + truncate bgray]
  where
    rgray = fromIntegral r * 0.3125 :: Double
    ggray = fromIntegral g * 0.5625 :: Double
    bgray = fromIntegral b * 0.125 :: Double
grayscale _ = wrongDimensionsError

grayscaleFilter :: Signal Int -> Signal Int
grayscaleFilter = mapSDF 3 1 grayscale

grayscaleFilterM :: Signal Int -> State (Int, Int) (Signal Int)
grayscaleFilterM img = return $ grayscaleFilter img

oddtruncation :: Int -> Int
oddtruncation x =
    if odd x
        then 1
        else 0

resizedSize :: Int -> Int
resizedSize x = (x - (oddtruncation x)) `div` 2

-- Splits even rows to first signal and odd rows to second signal.
resizeSplitter :: Int -> Signal Int -> (Signal Int, Signal Int)
resizeSplitter width = actor12SDF input (output, output) resizeSplit
  where
    input = 2 * halfinput
    halfinput = width
    -- We need to handle odd image size by truncating the last column
    output = halfinput - oddtruncation halfinput
    resizeSplit pixels = [(even_rows, odd_rows)]
      where
        even_rows = take output pixels
        odd_rows = take output $ drop halfinput pixels

resize :: [Int] -> [Int] -> [Int]
resize [p1,p2] [p3,p4] = [truncate (pixelsum / 4)]
  where
    pixelsum = fromIntegral $ p1 + p2 + p3 + p4 :: Double
resize _ _ = wrongDimensionsError

resizeOperation :: Signal Int -> Signal Int -> Signal Int
resizeOperation = zipWithSDF (2, 2) 1 resize

resizeImage :: Signal Int -> State (Int, Int) (Signal Int)
resizeImage img = do
    (width,height) <- get
    put (resizedSize width, resizedSize height)
    let (even_rows,odd_rows) = resizeSplitter width img
    return $ resizeOperation even_rows odd_rows

-- Calculates the sobel gradient, just make sure the input is in the right
-- order. So the left column for Gx should be in a,b,c and the right column
-- should be in d,e,f. Same for the upper and lower row in Gy respectively.
gradient
    :: [Int] -> [Int]
gradient [a,b,c,d,e,f] = [d + 2 * e + f - a - 2 * b - c]
gradient _ = wrongDimensionsError

gradientFilter :: Signal Int -> Signal Int
gradientFilter = mapSDF 6 1 gradient

magnitude :: [Int] -> [Int] -> [Int]
magnitude [gx] [gy] = [truncate mag]
  where
    mag = min 255 $ sqrt $ fromIntegral $ gx * gx + gy * gy :: Double
magnitude _ _ = wrongDimensionsError

magnitudeFilter :: Signal Int -> Signal Int -> Signal Int
magnitudeFilter = zipWithSDF (1, 1) 1 magnitude

threeRowSplitter :: Int -> [Int] -> [Int] -> [([Int], [Int], [Int], [Int])]
threeRowSplitter width r1r2 r3 = [(r2r3, r1, r2, r3)]
  where
    r1 = take width r1r2
    r2 = drop width r1r2
    r2r3 = r2 ++ r3

threeRowFeeder :: Int -> Signal Int -> (Signal Int, Signal Int, Signal Int)
threeRowFeeder width img = (r1, r2, r3)
  where
    (r2r3,r1,r2,r3) =
        actor24SDF
            (historySize, width)
            (historySize, width, width, width)
            (threeRowSplitter width)
            history
            img
    historySize = width * 2
    history = delaynSDF (replicate (2 * width) 0) r2r3

sobelPixelChooser :: [Int] -> [Int] -> [Int] -> [Int]
sobelPixelChooser (r11:r12:r13:r1xs) (r21:r22:r23:r2xs) (r31:r32:r33:r3xs) =
    [r11, r12, r13, r21, r23, r31, r32, r33] ++
    sobelPixelChooser (r12 : r13 : r1xs) (r22 : r23 : r2xs) (r32 : r33 : r3xs)
sobelPixelChooser _ _ _ = []

sobelFeeder :: Int -> Signal Int -> Signal Int -> Signal Int -> Signal Int
sobelFeeder width = zipWith3SDF (width, width, width) output sobelPixelChooser
  where
    output = (8 * (width - 2))

-- We assume the pixels come in the following grid:
-- abc
-- d e
-- fgh
sobelSplit
    :: [Int] -> [([Int], [Int])]
sobelSplit [a,b,c,d,e,f,g,h] = [([a, d, f, c, e, h], [a, b, c, f, g, h])]
sobelSplit _ = wrongDimensionsError

sobelSplitter :: Signal Int -> (Signal Int, Signal Int)
sobelSplitter = actor12SDF 8 (6, 6) sobelSplit

sobelFilter :: Signal Int -> State (Int, Int) (Signal Int)
sobelFilter img = do
    (width,height) <- get
    let newWidth = width - 2
        gx = gradientFilter gxsplit
        gy = gradientFilter gysplit
        (gxsplit,gysplit) = sobelSplitter s
        s = sobelFeeder width r1 r2 r3
        (r1,r2,r3) = threeRowFeeder width img
    put (newWidth, height - 2)
    return . dropS (newWidth * 2) $ magnitudeFilter gx gy

asciiLevels :: [Char]
asciiLevels = ['@','N','Q','B','#','A','V','E','S','t','/',';','=',',','_',' ']

grayscaleToAscii :: [Int] -> [Char]
grayscaleToAscii [x] = [asciiLevels !! level]
  where
    darknessLength = fromIntegral $ length asciiLevels
    level = truncate $ saturation * (darknessLength - 1)
    saturation = fromIntegral x / 255 :: Double
grayscaleToAscii _ = wrongDimensionsError

asciiArt :: Signal Int -> Signal Char
asciiArt = mapSDF 1 1 grayscaleToAscii

asciiArtM :: Signal Int -> State (Int, Int) (Signal Char)
asciiArtM img = return $ asciiArt img

printAsciiArt :: Int -> Signal Char -> IO ()
printAsciiArt _ NullS = return ()
printAsciiArt width img = do
    putStrLn . take width $ fromSignal img
    printAsciiArt width $ dropS width img

lab2ImageProcessing :: Signal Int -> State (Int, Int) (Signal Char)
lab2ImageProcessing img =
    grayscaleFilterM img >>= resizeImage >>= sobelFilter >>= asciiArtM
