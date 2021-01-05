module EdgeDetect where

import Definitions
import Grayscale

edgeDetect :: Image -> Image
edgeDetect = edgeDetectHelper . extendPic . grayscale

extendPic :: Image -> Image
extendPic img@(Image w h c) = Image {width = w + 2, height = h + 2, content = extendRGB c (c !! (h -1))}
  where
    extendRGB :: [[Rgb]] -> [Rgb] -> [[Rgb]]
    extendRGB mat@(x : xs) lastRow =
      let newFirstRow = doubleFirstAndLast x
          newLastRow = doubleFirstAndLast lastRow
          midRows = generateMidRows mat
       in (newFirstRow : midRows) ++ [newLastRow]

doubleFirstAndLast :: [a] -> [a]
doubleFirstAndLast lst@(h : t) = [h] ++ lst ++ findLastDoubleIt t
  where
    findLastDoubleIt [a] = [a]
    findLastDoubleIt (_ : t) = findLastDoubleIt t

generateMidRows :: [[Rgb]] -> [[Rgb]]
generateMidRows = map doubleFirstAndLast

--change
multiply :: Num a => [[a]] -> [[a]] -> [[a]]
multiply us vs = map (mult [] vs) us
  where
    mult xs [] _ = xs
    mult xs _ [] = xs
    mult [] (zs : zss) (y : ys) = mult (map (y *) zs) zss ys
    mult xs (zs : zss) (y : ys) = mult (zipWith (\u v -> u + v * y) xs zs) zss ys

operateWithMatrix :: [[Rgb]] -> [[Rgb]]
operateWithMatrix mat = toRgb $ clamp $ norm $ first `multiply` rgbToMat mat `multiply` second
  where
    first = [[1, 0, -1], [2, 0, -2], [1, 0, -1]]
    second = [[1, 2, 1], [0, 0, 0], [-1, -2, -1]]
    rgbToMat :: [[Rgb]] -> [[Float]]
    rgbToMat [] = []
    rgbToMat (x : xs) = rgbRowToFloatRow x : rgbToMat xs
      where
        rgbRowToFloatRow :: [Rgb] -> [Float]
        rgbRowToFloatRow [] = []
        rgbRowToFloatRow ((Rgb red _ _) : xs) = fromIntegral red : rgbRowToFloatRow xs

edgeDetectHelper :: Image -> Image
edgeDetectHelper img@(Image w h c) = img
