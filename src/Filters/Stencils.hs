{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Filters.Stencils
(
  gauss1,
  gauss2,
  edge,
  invert,
  sumStencilToNormalize
)
where

import Data.Array.Repa as Repa
import Data.Array.Repa.Stencil
import Data.Array.Repa.Stencil.Dim2
import Filters.Types

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

laplace :: Filter
laplace =
  [stencil2|  -1 -1 -1
              -1  8 -1
              -1 -1 -1 |]

edge :: Filter
edge =
  [stencil2|  0 1  0
              1 -4 1
              0 1  0 |]

invert :: Filter
invert =
  [stencil2| 0 1 0
              0 0 1
              0 1 0 |]

sumStencilToNormalize :: Filter -> Double
sumStencilToNormalize f = if result /= 0
                          then result
                          else 1
  where
    result = (mapStencil2 (BoundConst 1) f matrix)!(Z:.0:.0)
    matrix = fromFunction (Z :. 1 :. 1 ) (\(Z :. _ :. _) -> 1)
