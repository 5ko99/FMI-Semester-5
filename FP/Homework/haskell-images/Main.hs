import Data.Word (Word8)
import Definitions (Image (Image), Rgb (Rgb))
import EdgeDetect (edgeDetect)
import FloodFill (floodFill)
import Grayscale (grayscale)
import SaveAndLoad (loadImage, saveImage)

testIMG =
  Image
    3
    2
    [ [Rgb 255 0 0, Rgb 155 128 0, Rgb 255 255 0],
      [Rgb 0 255 0, Rgb 255 255 255, Rgb 128 255 128]
    ]

getRGB (Image w h c) = c

main = loadImage "test.ppm"