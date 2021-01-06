module FloodFill (floodFill) where

import Definitions (Image (..), Rgb)
import Grayscale (updateMatrix)

floodFillHelper :: (Int, Int) -> Rgb -> Rgb -> Image -> Image
floodFillHelper _ target replacement img@(Image w h mat)
  | target == replacement = img
floodFillHelper (x, y) _ _ img@(Image w h mat)
  | x >= h || y >= w || x < 0 || y < 0 = img
floodFillHelper (x, y) target replacement img@(Image w h mat)
  | (mat !! x !! y) /= target = img
floodFillHelper (x, y) target replacement img@(Image w h mat) = left
  where
    newMat = updateMatrix mat replacement (x, y)
    newImg = Image {width = w, height = h, content = newMat}
    up = floodFillHelper (x + 1, y) target replacement newImg
    down = floodFillHelper (x -1, y) target replacement up
    right = floodFillHelper (x, y + 1) target replacement down
    left = floodFillHelper (x, y -1) target replacement right

floodFill :: Rgb -> Int -> Int -> Image -> Image
floodFill color x y img@(Image w h c) = floodFillHelper (x, y) target color img
  where
    target = c !! x !! y
