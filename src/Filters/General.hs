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
  sumPixels
  ) where

import Control.Monad
import Data.Array.Repa as Repa hiding ((++))
import Codec.Picture
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Filters.Types
import IO.Arrays
import Filters.Stencils

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

getNeighbours :: Int -> Array D DIM2 RGBA8 -> DIM2 -> [RGBA8]
getNeighbours 0 _ _ = []
getNeighbours n matrix shape = getNeighbours' n1 n1 n1 matrix shape where
  n1 = n-1

sumPixels :: [RGBA8] -> (Int, Int, Int, Int)
sumPixels [] = (0, 0, 0, 0)
sumPixels ((r, g, b, a):xs) = (ir+r', ig+g', ib+b', ia+a') where
  (r', g', b', a') = sumPixels xs
  ir = fromIntegral r
  ig = fromIntegral g
  ib = fromIntegral b
  ia = fromIntegral a

meanPixels :: Int -> [RGBA8] -> RGBA8
meanPixels n l = (
  fromIntegral $ r `div` n2,
  fromIntegral $ g `div` n2,
  fromIntegral $ b `div` n2,
  fromIntegral $ a `div` n2) where
    (r, g, b, a) = sumPixels l
    n2 = n*n

meanNeighbours :: Int  -> Array D DIM2 RGBA8 -> DIM2 -> RGBA8
meanNeighbours i matrix shape = (r , g , b , a ) where
  (r, g, b, a) = meanPixels i (getNeighbours i matrix (getNth i matrix shape))

getNth :: Int -> Array D DIM2 RGBA8 -> DIM2 -> DIM2
getNth i matrix (Z :. w :. h) = (Z :. cw :. ch) where
  (mw, mh) = (\(Z :. a :. b) -> (a, b)) $ extent matrix
  cw = max 0 (min (i*w) (mw-1))
  ch = max 0 (min (i*h) (mh-1))

part :: Int -- jaką część macierzy wyciąć
        -> Array D DIM2 RGBA8 -> DIM2
part i matrix = f $ extent matrix where
  f = \(Z :. w :. h)-> (Z :. (w `div` i) :. (h `div` i))

sizeDown :: Int -- ilokrotnie zmniejszyć macierz
            -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
sizeDown n matrix = return $ fromFunction (part n matrix) (meanNeighbours n matrix)

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

toBlack :: DIM2 -> Array D DIM2 Pixel8
toBlack shape = fromFunction shape (\(Z :. _ :. _) -> 0)

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
