{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}
module Filters.General
(
  setAlpha,
  setTransparency,
  applyForP,
  applyFilterP,
  applyFor,
  applyFilter,
  processP,
  toDoubleP,
  toDouble,
  normalizeP,
  normalize
) where

import Control.Monad
import Data.Array.Repa as Repa
import Codec.Picture
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Filters.Types
import IO.Arrays
import Filters.Stencils

setAlpha :: Pixel8 -> Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
setAlpha newAlpha matrix = Repa.map
  (\(r, g, b, _) -> (r,g,b,newAlpha) )
  matrix

setTransparency :: Array D DIM2 RGBA8 -> IO (Array D DIM2 RGBA8)
setTransparency matrix =  do
                          putStrLn "Please specify transparency by typing number from 0 to 255."
                          putStrLn "(It may not be visible unless you had chosen .png extension.)"
                          alpha <- getLine
                          return (setAlpha (read alpha) matrix)

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
