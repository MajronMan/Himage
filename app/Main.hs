module Main where

import Lib

main :: IO ()
main = do
  path <- greeter
  img <- reader path
  let matrix = fromImageToRepa img
  --matrixP <- gaussianBlurOutsideFigureWithFrameP (Circle (Point 256 256) 150 ) matrix
  --matrixP <- extractColor Blue matrix
  --matrixP <- add matrix matrix
  putStrLn "How many times smaller image?"
  line <- getLine
  let i = read line
  matrixP <- sizeDown i matrix
  writer path (fromRepaToImage matrixP)
