module IO.Arrays
(
  zip4,
  unzip4,
  fromImageToRepa,
  fromRepaToImage
) where

import System.Environment
import Control.Monad
import Data.Array.Repa as Repa
import Codec.Picture
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Filters.Types

zip4 :: Array D DIM2 a -> Array D DIM2 a -> Array D DIM2 a -> Array D DIM2 a -> Array D DIM2 (a,a,a,a)
zip4 m1 m2 m3 m4 = fromFunction
  (extent m1)
  (\(Z:. w :. h) -> (m1!(Z:. w :. h) , m2!(Z:. w :. h) , m3!(Z:. w :. h) , m4!(Z:. w :. h)))


unzip4 :: Array D DIM2 (a,a,a,a) -> (Array D DIM2 a, Array D DIM2 a, Array D DIM2 a, Array D DIM2 a)
unzip4 matrix = (m1, m2, m3, m4)
  where m1 = unzip4' (\(a,b,c,d) -> a)
        m2 = unzip4' (\(a,b,c,d) -> b)
        m3 = unzip4' (\(a,b,c,d) -> c)
        m4 = unzip4' (\(a,b,c,d) -> d)
        unzip4' f = fromFunction
          (extent matrix)
          (\(Z:. w:. h) -> f (matrix!(Z:. w:. h)))

fromImage :: (Pixel a) => Image a -> (a -> RGBA8) -> Array D DIM2 RGBA8
fromImage image f =
  fromFunction
  ( Z :. width :. height )
  (\(Z :. w :. h) -> f $
   (pixelAt  image  w h))
  where width = imageWidth image
        height = imageHeight image

fromImageToRepa :: DynamicImage -> Array D DIM2 RGBA8
fromImageToRepa image  = fromImage (convertRGBA8 image) fromRGBA8

fromRepaToImage :: Array D DIM2 RGBA8-> DynamicImage
fromRepaToImage matrix =
  ImageRGBA8 (
    generateImage
    (\w h -> toRGBA8 $
    matrix!(Z :. w :. h))
    width
    height)
  where (Z :. width :. height) = extent matrix
