{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}


module Filters
    (
    setTransparency,
    ) where

import System.Environment
import Data.Array.Repa as Repa
import Codec.Picture

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
