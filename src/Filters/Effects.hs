{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}
-----
-- functionP is parallel evaluation function
-----
module Filters.Effects
    (
    detectEdgeP,
    gaussianBlurP,
    detectEdge,
    gaussianBlur,
    inversionP,
    setTransparency,
    grayscale,
    gaussianBlurOutsideFigureWithFrameP,
    edgeInsideFigureP
    ) where

import Control.Monad
import Data.Array.Repa as Repa
import Data.Array.Repa.Stencil.Dim2

import Filters.Stencils
import Filters.General
import Filters.Types
import Filters.Figures

detectEdgeP :: Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
detectEdgeP matrix =   (applyForP 3 gauss2 >=> applyFilterP edge) matrix

gaussianBlurP :: Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
gaussianBlurP = applyFilterP gauss2

inversionP :: Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
inversionP = applyFilterP invert

detectEdge :: Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
detectEdge matrix = applyFor 3 gauss2 .
                    applyFilter edge $
                    matrix

gaussianBlur :: Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
gaussianBlur = applyFilter gauss1

setTransparency :: Array D DIM2 RGBA8 -> IO (Array D DIM2 RGBA8)
setTransparency matrix =  do
                          putStrLn "Please specify transparency by typing number from 0 to 255."
                          putStrLn "(It may not be visible unless you had chosen .png extension.)"
                          alpha <- getLine
                          return (setAlpha (read alpha) matrix)

grayscale :: Array D DIM2 RGBA8 -> IO (Array D DIM2 RGBA8)
grayscale matrix = do
                      matrix <- desaturationP matrix
                      return (delay matrix)


noFilter :: Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
noFilter matrix = do
                  return matrix

gaussianBlurOutsideFigureWithFrameP :: Figure -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
gaussianBlurOutsideFigureWithFrameP figure matrix =
  do
  filtered <- ( applyPartiallyInFigureP figure noFilter gaussianBlurP >=>
                applyPartiallyInFigureP (enlarge figure (1.5 * (radius figure))) noFilter gaussianBlurP >=>
                applyPartiallyInFigureP (enlarge figure (2.5 * (radius figure))) noFilter gaussianBlurP >=>
                applyPartiallyInFigureP (enlarge figure (3.5 * (radius figure))) noFilter gaussianBlurP)
                matrix
  return (addFrame figure 5 (0,0,0,255) filtered)

edgeInsideFigureP :: Figure -> Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
edgeInsideFigureP figure = applyPartiallyInFigureP figure detectEdgeP noFilter
