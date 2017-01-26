{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Filters.Types
  (
  RGBA8,
  Filter,
  Color(Red, Green, Blue),
  fromRGBA8,
  toRGBA8,
  addRGBA8
  ) where

import Data.Array.Repa as Repa
import Codec.Picture
import Data.Array.Repa.Stencil

type RGBA8 = (Pixel8,Pixel8,Pixel8,Pixel8)

type Filter = Stencil DIM2 Double

-- |
-- Convert pixels from Juicy Pixels PixelRGBA8 representation
fromRGBA8 :: PixelRGBA8 -> RGBA8
fromRGBA8 (PixelRGBA8 r g b alpha) = (r,g,b,alpha)

-- |
-- Convert pixels to Juicy Pixels PixelRGBA8 representation
toRGBA8 :: RGBA8 -> PixelRGBA8
toRGBA8 (r, g, b, alpha) = (PixelRGBA8 r g b alpha)

-- |
-- Simple addition of two pixels with alpha channel
addRGBA8 :: RGBA8 -> RGBA8 -> RGBA8
addRGBA8 (r,b,g,alpha) (r',b',g',alpha') = (r+r',b+b',g+g',alpha+alpha')

data Color = Red | Green | Blue deriving (Eq)
