module Main
where
  import Lib
  import Test.Hspec
  import Data.Typeable
  import Data.Array.Repa
  import Codec.Picture

  main :: IO ()
  main = hspec $ do
    describe "Load image" $ do
      it "reads from file" $ do
        img <- readImage "lena512.bmp"
        case img of
          Left err -> error err
          Right image ->  do
                            let arr = fromImageToRepa image
                            typeOf arr `shouldBe` Array D DIM2 RGBA8
