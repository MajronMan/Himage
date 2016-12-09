{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}


module Lib
    (
    greeter,
    reader,
    writer
    ) where

import System.Environment
import Data.List
import Data.Array.Repa hiding ((++))
import Codec.Picture

data ModifiedFileInfo = ModifiedFileInfo
                        {
                          dir :: String,
                          inName :: String,
                          inType :: String,
                          outName :: String,
                          outType :: String
                        }

instance Show ModifiedFileInfo where
  show a = dir a ++ " (" ++ inName a++inType a ++ " -> " ++ outName a ++ outType a ++")"

splitFileName' :: String -> String -> (String, String)
splitFileName' "" acc = (acc,"")
splitFileName' (x:xs) res = if x == '.'
                            then (res, xs)
                            else splitFileName' xs (res++[x])
splitFileName :: String -> (String, String)
splitFileName s = splitFileName' s ""

greeter :: IO ModifiedFileInfo
greeter = do
          putStrLn "Please specify image directory path"
          directory <- getLine
          putStrLn "Please specify input image name (with extension)"
          inFile <- getLine
          putStrLn "Please specify output image name (with extension)"
          outFile <- getLine
          let (inFileName, inExtension) = splitFileName inFile
          let (outFileName, outExtension) = splitFileName outFile
          return ModifiedFileInfo {
                                  dir=directory,
                                  inName=inFileName,
                                  inType=inExtension,
                                  outName=outFileName,
                                  outType=outExtension
                                  }

reader :: ModifiedFileInfo -> IO DynamicImage
reader info = do
              let path = dir info ++"/"++ inName info ++ "." ++inType info
              ret <- readImage path
              case ret of
                Left err -> error err
                Right img ->  return img

writer :: ModifiedFileInfo -> DynamicImage -> IO()
writer info image = do
                    let path = dir info ++"/"++ outName info ++ "." ++ outType info
                    case outType info of
                      "png" -> savePngImage (path) image
                      "bmp" -> saveBmpImage (path) image
                      "jpg" -> saveJpgImage 100 (path) image
                    putStrLn ("Image written at: "++path)
