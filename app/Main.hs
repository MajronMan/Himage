module Main where

import Lib
import System.Environment

main :: IO ()
main = do
   path <- greeter
   img <- reader path
   writer path img
