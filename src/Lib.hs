
{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}


module Lib
    (
    greeter,
    reader,
    writer,
    fromImageToRepa,
    fromRepaToImage,
    detectEdgeP,
    inversionP,
    gaussianBlurP
    ) where

import IO.Files
import IO.Arrays
import Filters.Effects
