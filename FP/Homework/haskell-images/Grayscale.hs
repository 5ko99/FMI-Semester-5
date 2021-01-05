module Grayscale where

import Definitions (Image (..), Rgb (..))

updateMatrix :: [[Rgb]] -> Rgb -> (Int, Int) -> [[Rgb]]
updateMatrix mat x (row, col) =
  take row mat
    ++ [take col (mat !! row) ++ [x] ++ drop (col + 1) (mat !! row)]
    ++ drop (row + 1) mat

multiplyPixel :: Rgb -> Float -> Float -> Float -> Rgb
multiplyPixel (Rgb r g b) rm gm bm =
  let newVal = round (rm * fromIntegral r :: Float) + round (gm * fromIntegral g :: Float) + round (bm * fromIntegral b :: Float)
   in Rgb {red = newVal, green = newVal, blue = newVal}

grayscale :: Image -> Image
grayscale img@(Image _ _ []) = img
grayscale img = grayscaleHelper img 0 0
  where
    grayscaleHelper img@(Image w h c) i j | i == h && j == w = img
    grayscaleHelper img@(Image w h c) i j | i == h = img
    grayscaleHelper img@(Image w h c) i j | j == w = grayscaleHelper img (i + 1) 0
    grayscaleHelper img@(Image w h c) i j =
      grayscaleHelper
        Image
          { width = w,
            height = h,
            content = updateMatrix c (multiplyPixel (c !! i !! j) rm gm bm) (i, j)
          }
        i
        (j + 1)
      where
        rm = 0.30
        gm = 0.59
        bm = 0.11
