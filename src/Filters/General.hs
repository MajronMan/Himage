{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}
module Filters.General
  (
  setAlpha,
  desaturationP,
  applyForP,
  applyFilterP,
  applyFor,
  applyFilter,
  processP,
  toDoubleP,
  toDouble,
  normalizeP,
  normalize,
  extractColor,
  add,
  sizeDown,
  getNeighbours,
  meanNeighbours,
  meanPixels,
  sumPixels,
  sizeUp,
  brutalSizeUp,
  overlay
  ) where

import Control.Monad
import Data.Array.Repa as Repa hiding ((++))
import Codec.Picture
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Filters.Types
import IO.Arrays
import Filters.Stencils

-- |
-- Alias for Repa.append
add :: Array D DIM2 RGBA8 -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
add m1 m2 = return $ Repa.append m1 m2

getNeighbours' :: Int -> Int -> Int -> Array D DIM2 RGBA8 -> DIM2 -> [RGBA8]
getNeighbours' 0 0 _ matrix shape = [index matrix shape]
getNeighbours' x (-1) width matrix shape = getNeighbours' (x-1) width width matrix shape
getNeighbours' x y width matrix (Z :. w :. h) =
  [index matrix (Z :. cw :. ch)] ++
  getNeighbours' x (y-1) width matrix (Z :. w :. h) where
    (mw, mh) = (\(Z :. a :. b) -> (a, b)) $ extent matrix
    cw = min (w+x) (mw-1)
    ch = min (h+y) (mh-1)
-- |
-- Returns neighbours of given pixel 
getNeighbours :: Int -> Array D DIM2 RGBA8 -> DIM2 -> [RGBA8]
getNeighbours 0 _ _ = []
getNeighbours n matrix shape = getNeighbours' n1 n1 n1 matrix shape where
  n1 = n-1

-- |
-- Returns quadruple containing summed components of pixels
sumPixels :: [RGBA8] -> (Int, Int, Int, Int)
sumPixels [] = (0, 0, 0, 0)
sumPixels ((r, g, b, a):xs) = (ir+r', ig+g', ib+b', ia+a') where
  (r', g', b', a') = sumPixels xs
  ir = fromIntegral r
  ig = fromIntegral g
  ib = fromIntegral b
  ia = fromIntegral a

-- |
-- Count an average of a group of pixels
meanPixels :: Int -> [RGBA8] -> RGBA8
meanPixels n l = (
  fromIntegral $ r `div` n2,
  fromIntegral $ g `div` n2,
  fromIntegral $ b `div` n2,
  fromIntegral $ a `div` n2) where
    (r, g, b, a) = sumPixels l
    n2 = n*n

-- |
-- Calculate average values from neighbours of the given pixel
meanNeighbours :: Int  -> Array D DIM2 RGBA8 -> DIM2 -> RGBA8
meanNeighbours i matrix shape = (r , g , b , a ) where
  (r, g, b, a) = meanPixels i (getNeighbours i matrix (getNth i matrix shape))

-- |
-- Return nth pixel from given array
getNth :: Int -> Array D DIM2 RGBA8 -> DIM2 -> DIM2
getNth i matrix (Z :. w :. h) = (Z :. cw :. ch) where
  (mw, mh) = (\(Z :. a :. b) -> (a, b)) $ extent matrix
  cw = max 0 (min (i*w) (mw-1))
  ch = max 0 (min (i*h) (mh-1))

-- |
-- Takes an array and returns just a part of it
part :: Int -- ^ how much to cut
        -> Array D DIM2 RGBA8 -> DIM2
part i matrix = f $ extent matrix where
  f = \(Z :. w :. h)-> (Z :. (w `div` i) :. (h `div` i))

-- |
-- Resize array n times
sizeDown :: Int -- ^ how many times smaller
            -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
sizeDown n matrix = return $ fromFunction (part n matrix) (meanNeighbours n matrix)

-- |
-- Return shape describing n times bigger array
extendMatrix :: Int -> Array D DIM2 RGBA8 -> DIM2
extendMatrix i matrix = f $ extent matrix where
  f = \(Z :. w :. h) -> (Z :. (w * i) :. (h*i))

-- |
-- Increase size of array by simply copying pixels
brutalSizeUp :: Int -- ilokrotnie zwiększyć macierz
            -> Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
brutalSizeUp n matrix = fromFunction (extendMatrix n matrix) f where
  f = \(Z :. w :. h) -> index matrix (Z :. (w `div` n) :. (h `div` n))

-- |
-- Increase size of array using bilinear interpolation
sizeUp :: Int -- ilokrotnie zwiększyć macierz
            -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
sizeUp n matrix = return (fromFunction (extendMatrix n matrix) f) where
  f = \shape -> meanPixels n (getNeighbours n (brutalSizeUp n matrix) shape)

-- |
-- Change transparency of all pixels
setAlpha :: Pixel8 -> Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
setAlpha newAlpha matrix = Repa.map
  (\(r, g, b, _) -> (r,g,b,newAlpha) )
  matrix

desaturationP :: Array D DIM2 RGBA8 -> IO(Array U DIM2 RGBA8)
desaturationP matrix =  computeP $ Repa.traverse matrix id luminosity

luminosity :: (DIM2 -> RGBA8) -> DIM2 -> RGBA8
luminosity f (Z :. i :. j) = (x,x,x,alpha)
  where
    a1 = 0.21 :: Double
    a2 = 0.71 :: Double
    a3 = 0.07 :: Double
    x = ceiling $ a1 *(fromIntegral r) + a2 * (fromIntegral g) + a3 * (fromIntegral b)
    (r,g,b,alpha) = f (Z :. i :. j)

-- |
-- Create a black image of given shape
toBlack :: DIM2 -> Array D DIM2 Pixel8
toBlack shape = fromFunction shape (\(Z :. _ :. _) -> 0)

-- |
-- Set components of each pixel to 0 except for given colour
extractColor :: Color -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
extractColor color matrix
  | color == Red = return $ zip4 r n n a
  | color == Green = return $ zip4 n g n a
  | color == Blue = return $ zip4 n n b a
  where
    n = toBlack (extent matrix)
    (r, g, b, a) = unzip4 matrix

--------------------------------------------------------------------------------
------------------------------równoległe----------------------------------------
--------------------------------------------------------------------------------

processP :: Filter -> Array U DIM2 Double -> IO(Array U DIM2 Double)
processP f matrix = computeP (mapStencil2 (BoundConst 0) f matrix )

toDoubleP :: Array D DIM2 Pixel8 -> IO(Array U DIM2 Double)
toDoubleP = computeP . Repa.map (\x -> fromIntegral (fromIntegral x :: Int))

fromDoubleP :: Array U DIM2 Double -> IO(Array U DIM2 Pixel8)
fromDoubleP = computeP . Repa.map (\x -> fromIntegral (truncate x :: Int))

normalizeP :: Double -> Array U DIM2 Double -> IO(Array U DIM2 Double)
normalizeP x matrix = computeP (Repa.map (/x) matrix)

applyFilterP :: Filter -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
applyFilterP f matrix =  do
                        let (rArray, gArray, bArray, alphaArray) = unzip4 matrix
                        [r, g, b] <- mapM ( toDoubleP >=>
                                            processP f >=>
                                            normalizeP (sumStencilToNormalize f) >=>
                                            fromDoubleP)
                                            [rArray, gArray, bArray]
                        return (zip4 (delay r) (delay g) (delay b) alphaArray)

applyForP :: Int -> Filter -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
applyForP 0 _ matrix = return( matrix )
applyForP n f matrix =  do
                        matrix' <- applyFilterP f matrix
                        matrix'' <- applyForP (n-1) f matrix'
                        return (matrix'')

--------------------------------------------------------------------------------
---------------------------nierównoległe----------------------------------------
--------------------------------------------------------------------------------
toDouble :: Array D DIM2 Pixel8 -> Array D DIM2 Double
toDouble = Repa.map (\x -> fromIntegral (fromIntegral x :: Int))

fromDouble :: Array D DIM2 Double -> Array D DIM2 Pixel8
fromDouble = Repa.map (\x -> fromIntegral (truncate x :: Int))

normalize :: Double -> Array D DIM2 Double -> Array D DIM2 Double
normalize x =  Repa.map (/x)

applyFilter :: Filter -> Array D DIM2 RGBA8 ->  Array D DIM2 RGBA8
applyFilter f matrix = zip4 rArray' gArray' bArray' alphaArray
  where [rArray', gArray', bArray'] =
          Prelude.map
          ( fromDouble .
            normalize (sumStencilToNormalize f) .
            delay .
            (mapStencil2 (BoundConst 0) f) .
            toDouble)
          [rArray, gArray, bArray]
        (rArray, gArray, bArray, alphaArray) = unzip4 matrix

applyFor :: Int -> Filter -> Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
applyFor 0 _ matrix = matrix
applyFor n f matrix =  applyFor (n-1) f . applyFilter f $ matrix

-- | 
-- Like zipWith, but for pixels
zipWithRGBA8 :: RGBA8 -> RGBA8 -> (Pixel8 -> Pixel8 -> Pixel8) -> RGBA8
zipWithRGBA8 (r1, g1, b1, a1) (r2, g2, b2, a2) f = (f r1 r2, f g1 g2, f b1 b2, f a1 a2)

-- |
-- Calculate sum of two pixels using "overlay" method 
overlayPixel :: Pixel8 -> Pixel8 -> Pixel8
overlayPixel p1 p2
  | a < 0.5 = round (2*a*b*255)  :: Pixel8
  | otherwise = round ((1-2*(1-a)*(1-b))*255) :: Pixel8
  where
    a = fromIntegral p1 / 255 :: Double
    b = fromIntegral p2 / 255 :: Double

-- |
-- Use overlay function to add two images
overlay :: Array D DIM2 RGBA8 -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
overlay base top = return $ fromFunction (extent base) f where
  (Z:. tw :. th) = extent top
  f = \(Z :. w :. h) -> if and [tw > w, th > h]
    then zipWithRGBA8 (base ! (Z:.w:.h)) (top ! (Z:.w:.h)) overlayPixel
    else base ! (Z:.w:.h)
