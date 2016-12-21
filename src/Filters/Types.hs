{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}

module Filters.Types
(
  RGBA8,
  Filter,
  fromRGBA8,
  toRGBA8
)
where

import Data.Array.Repa as Repa
import Codec.Picture
import Data.Array.Repa.Stencil

type RGBA8 = (Pixel8,Pixel8,Pixel8,Pixel8)

type Filter = Stencil DIM2 Double

fromRGBA8 :: PixelRGBA8 -> RGBA8
fromRGBA8 (PixelRGBA8 r g b alpha) = (r,g,b,alpha)

toRGBA8 :: RGBA8 -> PixelRGBA8
toRGBA8 (r, g, b, alpha) = (PixelRGBA8 r g b alpha)
