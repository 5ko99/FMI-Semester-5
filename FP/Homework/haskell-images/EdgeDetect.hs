module EdgeDetect (edgeDetect) where

import Definitions (Image (..), Rgb (..))
import Grayscale

edgeDetect :: Image -> Image
edgeDetect img@(Image w h _) = Image {width = w, height = h, content = matrix}
  where
    extendedPic = extendPic img
    list = map operateWithMatrix (generateMatrixForEachPixel extendedPic)
    intToRgb [] = []
    intToRgb (n : t) = Rgb {red = fromIntegral n, green = fromIntegral n, blue = fromIntegral n} : intToRgb t
    matrix = toMatrix w (intToRgb list)

toMatrix :: Int -> [a] -> [[a]]
toMatrix _ [] = []
toMatrix n lst = getN n lst : toMatrix n (removeN n lst)
  where
    getN 0 _ = []
    getN n (h : t) = h : getN (n -1) t
    removeN 0 lst = lst
    removeN n (h : t) = removeN (n -1) t

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

-- multi
multiplyScalar :: [[Float]] -> [[Float]] -> Float
multiplyScalar [r1, r2, r3] [r'1, r'2, r'3] =
  multiplyRow r3 r'1 + multiplyRow r2 r'2 + multiplyRow r1 r'3
  where
    multiplyRow :: [Float] -> [Float] -> Float
    multiplyRow [a1, b1, c1] [a2, b2, c2] = c1 * a2 + b1 * b2 + a1 * c2

operateWithMatrix :: [[Rgb]] -> Int
operateWithMatrix mat = floor clamped
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
    clamp x | x > 255 = 255
    clamp x | x < 0 = 0
    clamp x = x
    a = rgbToMat mat
    x = first `multiplyScalar` a
    y = second `multiplyScalar` a
    norm = sqrt (x * x + y * y)
    clamped = clamp norm

generateMatrix3x3 :: (Int, Int) -> [[Rgb]] -> [[Rgb]]
generateMatrix3x3 (x, y) mat = res
  where
    res =
      [ [mat !! (x + 1) !! (y -1), mat !! (x + 1) !! y, mat !! (x + 1) !! (y + 1)],
        [mat !! x !! (y - 1), mat !! x !! y, mat !! x !! (y + 1)],
        [mat !! (x -1) !! (y -1), mat !! (x -1) !! y, mat !! (x -1) !! (y + 1)]
      ]

generateMatrixForEachPixel :: Image -> [[[Rgb]]]
generateMatrixForEachPixel imgResized@(Image w h c) =
  [generateMatrix3x3 (x, y) c | x <- [1 .. h -2], y <- [1 .. w -2]]
