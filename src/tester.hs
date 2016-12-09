----------------------------------
-- for testing single functions --
----------------------------------
import System.Environment
import Data.List
import Data.Array.Repa.IO.BMP
import Data.Array.Repa                hiding ((++))

greeter :: IO String
greeter = do
          putStrLn "Please specify image path"
          path <- getLine
          return path

reader :: FilePath -> IO()
reader fp = do
            putStrLn fp
