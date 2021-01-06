module Definitions where

import Data.Word (Word8)

data Rgb = Rgb
  { red :: Word8,
    green :: Word8,
    blue :: Word8
  }
  deriving (Show, Read, Eq)

data Image = Image
  { width :: Int,
    height :: Int,
    content :: [[Rgb]]
  }
  deriving (Show, Read, Eq)