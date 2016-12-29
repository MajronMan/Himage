module Main where

import Lib

main :: IO ()
main = do
  path <- greeter
  img <- reader path
  let matrix = fromImageToRepa img
  matrixP <- gaussianBlurOutsideFigureWithFrameP (Circle (Point 256 256) 150 ) matrix
  writer path (fromRepaToImage matrixP)
