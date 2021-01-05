module SaveAndLoad where

import Data.List.Split (splitOn)
import Data.Maybe
import Definitions

saveImage :: FilePath -> Image -> IO ()
saveImage filePath (Image w h c) =
  writeFile filePath img
  where
    img = "P3 \n" ++ show w ++ " " ++ show h ++ "\n 255 \n" ++ rgbToString c

rgbToString :: [[Rgb]] -> String
rgbToString rgb = helper (concat rgb)
  where
    helper [] = []
    helper ((Rgb r g b) : t) = (show r ++ " " ++ show g ++ " " ++ show b ++ "\n") ++ helper t

--Load logic

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where
    (as, bs) = splitAt n xs

getContent :: [String] -> Int -> [[Rgb]]
getContent lst w = helper (splitEvery (w * 3) lst)
  where
    helper [] = []
    helper (h : t) = rowToRgb h : helper t
      where
        rowToRgb [] = []
        rowToRgb (r : g : b : t) = Rgb {red = read r, green = read g, blue = read b} : rowToRgb t

ensureIsP3 :: String -> Bool
ensureIsP3 str = str == "P3"

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace replaceThis with (h : t) | h == replaceThis = with : replace replaceThis with t
replace replaceThis with (h : t) | h /= replaceThis = h : replace replaceThis with t

clearEmpty :: [String] -> [String]
clearEmpty [] = []
clearEmpty (h : t) | h == "" = clearEmpty t
clearEmpty (h : t) | h /= "" = h : clearEmpty t

loadImage :: String -> IO Image
loadImage path = do
  img <- readFile path
  let split = clearEmpty $ splitOn " " (replace '\n' ' ' img)
   in if ensureIsP3 (head split)
        then
          let w = read $ split !! 1
              h = read $ split !! 2
              c = getContent (drop 4 split) w
           in return $ Image {width = w, height = h, content = c}
        else return Image {width = 0, height = 0, content = []}

test = "P3 \n3 2\n 255 \n255 0 0\n155 128 0\n255 255 0\n0 255 0\n255 255 255\n128 255 128\n"