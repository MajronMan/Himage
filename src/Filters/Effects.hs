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
    inversionP
    ) where

import Control.Monad
import Data.Array.Repa as Repa
import Data.Array.Repa.Stencil.Dim2

import Filters.Stencils
import Filters.General
import Filters.Types

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
