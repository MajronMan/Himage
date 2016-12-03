module Main where

import Lib
import System.Environment

main :: IO ()
main
 = do   args    <- getArgs
        case args of
         [iterations, fileIn, fileOut]  -> run (read iterations) fileIn fileOut
         _                              -> usage

usage :: IO()
usage   = putStr $ unlines
        [ "repa-blur <iterations::Int> <fileIn.bmp> <fileOut.bmp>" ]
