module Main where

import Lib

main :: IO ()
main = do
  path <- greeter
  img <- reader path
  let matrix = fromImageToRepa img
  matrixP <- detectEdgeP matrix
  writer path (fromRepaToImage matrixP)
