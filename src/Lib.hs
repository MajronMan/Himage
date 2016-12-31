
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
    gaussianBlurP,
    setTransparency,
    grayscale,
    gaussianBlurOutsideFigureWithFrameP,
    edgeInsideFigureP,
    Figure(Circle, Square, Diamond),
    Point(Point),
    Color(Red, Green, Blue),
    extractColor,
    add,
    sizeDown,
    ) where

import IO.Files
import IO.Arrays
import Filters.Effects
import Filters.Types
import Filters.Figures
import Filters.General
