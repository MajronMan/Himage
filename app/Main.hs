module Main where

import Lib

main :: IO ()
main = greeter
  -- path <- greeter
  -- img <- reader path
  -- let matrix = fromImageToRepa img
  -- --matrixP <- gaussianBlurOutsideFigureWithFrameP (Circle (Point 256 256) 150 ) matrix
  -- --matrixP <- extractColor Blue matrix
  -- --matrixP <- add matrix matrix
  -- -- putStrLn "How many times bigger image?"
  -- -- line <- getLine
  -- -- let i = read line :: Int
  -- -- matrixP <- sizeUp i matrix
  -- matrixP <- detectEdgeP matrix
  -- writer path (fromRepaToImage matrixP)
