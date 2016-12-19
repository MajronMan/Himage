{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}
-----
-- functionP is parallel evaluation function
-----
module Filters
    (
    setTransparency,
    applyFilterP,
    applyForP,
    detectEdgeP,
    gaussianBlurP,
    applyFilter,
    applyFor,
    detectEdge,
    gaussianBlur
    ) where

import System.Environment
import Control.Monad
import Data.Array.Repa as Repa
import Codec.Picture
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2

type RGBA8 = (Pixel8,Pixel8,Pixel8,Pixel8)

setAlpha :: Pixel8 -> Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
setAlpha newAlpha matrix = Repa.map
  (\(r, g, b, alpha) -> (r,g,b,newAlpha) )
  matrix

setTransparency :: Array D DIM2 RGBA8 -> IO (Array D DIM2 RGBA8)
setTransparency matrix =  do
                          putStrLn "Please  specify transparency by typing number from 0 to 255."
                          putStrLn "(It could not be visible unless you had choosen .png extension.)"
                          alpha <- getLine
                          return (setAlpha (read alpha) matrix)

type Filter = Stencil DIM2 Double

detectEdgeP :: Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
detectEdgeP matrix =   (applyForP 3 gauss2 >=> applyFilterP edge) matrix

gaussianBlurP :: Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
gaussianBlurP = applyFilterP gauss2

detectEdge :: Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
detectEdge matrix = applyFor 3 gauss2 .
                    applyFilter edge $
                    matrix

gaussianBlur :: Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
gaussianBlur = applyFilter gauss1

gauss2 :: Filter
gauss2 =
  [stencil2|  1 1 2 1 1
              1 2 4 2 1
              2 4 8 4 2
              1 2 4 2 1
              1 1 2 1 1 |]

gauss1 :: Filter
gauss1 =
  [stencil2|  1 2 1
              2 4 2
              1 2 1  |]

edge :: Filter
edge =
  [stencil2|  0 1  0
              1 -4 1
              0 1  0 |]

sumStencilToNormalize :: Filter -> Double
sumStencilToNormalize f = if result /= 0
                          then result
                          else 1
  where
    result = (mapStencil2 (BoundConst 1) f matrix)!(Z:.0:.0)
    matrix = fromFunction (Z :. 1 :. 1 ) (\(Z :. w :. h) -> 1)

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
applyForP 0 f matrix = return( matrix )
applyForP n f matrix =  do
                        matrix <- applyFilterP f matrix
                        matrix <- applyForP (n-1) f matrix
                        return (matrix)

--nierównoległe
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
applyFor 0 f matrix = matrix
applyFor n f matrix =  applyFor (n-1) f . applyFilter f $ matrix
