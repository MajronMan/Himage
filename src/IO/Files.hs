module IO.Files
    (
    greeter
    ) where

import System.Environment
import Data.List
import Data.Array.Repa hiding ((++))
import Codec.Picture
import Filters.General
import Filters.Effects
import Filters.Figures
import Filters.Types
import IO.Arrays

data ModifiedFileInfo = ModifiedFileInfo
                        {
                          dir :: String,
                          inName :: String,
                          inType :: String,
                          outName :: String,
                          outType :: String,
                          function :: Array D DIM2 RGBA8 -> IO(Array D DIM2 RGBA8)
                        }

instance Show ModifiedFileInfo where
  show a = dir a ++ " (" ++ inName a++ "." ++inType a ++ " -> " ++ outName a ++ "."++ outType a ++")"

splitFileName' :: String -> String -> (String, String)
splitFileName' "" acc = (acc,"")
splitFileName' (x:xs) res = if x == '.'
                            then (res, xs)
                            else splitFileName' xs (res++[x])

splitFileName :: String -> (String, String)
splitFileName s = splitFileName' s ""

inputFile :: IO ((DynamicImage, ModifiedFileInfo))
inputFile = do
            putStrLn "Please specify image directory path"
            directory <- getLine
            putStrLn "Please specify input image name (with extension)"
            inFile <- getLine
            putStrLn "Please specify output image name (with extension)"
            outFile <- getLine
            let (inFileName, inExtension) = splitFileName inFile
            let (outFileName, outExtension) = splitFileName outFile
            let mfInfo = ModifiedFileInfo {
                                    dir=directory,
                                    inName=inFileName,
                                    inType=inExtension,
                                    outName=outFileName,
                                    outType=outExtension
                                    }
            let path = dir mfInfo ++"/"++ inName mfInfo ++ "." ++inType mfInfo
            ret <- readImage path
            case ret of
              Left err -> do
                          putStrLn "No such image, try with different one"
                          inputFile
              Right img ->  return (img, mfInfo)

selectFunction :: (DynamicImage, ModifiedFileInfo) -> IO ((DynamicImage, ModifiedFileInfo))
selectFunction (img, mfi) =  do
  putStrLn "Please select a function: \n\
  \ 1. Gaussian blur\n \
  \ 2. Detect edge\n \
  \ 3. Desaturation\n \
  \ 4. Make blue\n \
  \ 5. Size up twice\n \
  \ 6. Size down twice\n \
  \ 7. Detect edge inside of a square\n \
  \ 8. Gaussian blur outside of a circle\n"
  option <- getLine
  let function = case option of
                    "1" -> gaussianBlurP
                    "2" -> detectEdgeP
                    "3" -> grayscale
                    "4" -> extractColor Blue
                    "5" -> sizeUp 2
                    "6" -> sizeDown 2
                    "7" -> edgeInsideFigureP (Square (Point 256 256) 150)
                    _ ->  gaussianBlurOutsideFigureWithFrameP (Circle (Point 256 256) 150 )
  modifiedImage <- function $ fromImageToRepa img
  return (fromRepaToImage modifiedImage, mfi)

writer :: (DynamicImage, ModifiedFileInfo) -> IO()
writer (image, info) = do
                    let path = dir info ++"/"++ outName info ++ "." ++ outType info
                    case outType info of
                      "bmp" -> saveBmpImage (path) image
                      "jpg" -> saveJpgImage 100 (path) image
                      "png" -> savePngImage (path) image
                      _     ->  do
                                  let path = dir info ++"/"++ outName info ++ ".png"
                                  savePngImage (path) image

                    putStrLn ("Image written at: "++ path)


greeter :: IO()
greeter = inputFile >>= selectFunction >>= writer
