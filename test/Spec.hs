module Main
where
  import Lib
  import Test.Hspec
  import Test.QuickCheck
  import Data.Typeable
  import Data.Array.Repa
  import Codec.Picture
  import Filters.Figures
  import IO.Arrays
  import IO.Files

  type FigureTester = (Point -> Double -> Figure) -> Int ->Int ->Double ->Int  -> Int -> Bool

  normTester :: FigureTester
  normTester f a b c d e =  (norm figure point == 0) == (center figure  == point)
    where figure = ( f (Point a b) c )
          point = (Point d e)

  insideOutsideTester :: FigureTester
  insideOutsideTester f a b c d e = (inside figure point) == (not (outside figure point ))
    where figure = ( f (Point a b) c )
          point = (Point d e)

  noFrameTester :: FigureTester
  noFrameTester  f  a b c d e  = (insideFrame figure 0 point ) == False
    --outside figure point)--,(norm figure point <= radius figure + width)])
    where figure = ( f (Point a b) c )
          point = (Point d e)

  imageRepaTester :: Array D DIM2 RGBA8 -> Bool
  imageRepaTester image = (fromImageToRepa . fromRepaToImage $ image)==(image)

  main :: IO ()
  main = hspec $ do
    describe "|p-q| = 0 <=> p=q" $ do
      it "calculate circle norm" . property $
        normTester Circle
      it "calculate square norm" . property $
        normTester Square
      it "calculate diamond norm" . property $
        normTester Diamond

    describe "(!inside => ouotside) and (!outside => inside)" $ do
      it "decide if inside or outside circle" . property $
        insideOutsideTester Circle
      it "decide  if inside or outside square" . property $
        insideOutsideTester Square
      it "decide if inside or outside diamond" . property  $
        insideOutsideTester Diamond

    describe "point can't be inside frame with width=0 " $ do
      it "can't be inside circle nofram" . property $
        noFrameTester Circle
      it "can't be inside squares noframe" . property $
        noFrameTester Square
      it "can't be inside diamonds noframe" . property  $
        noFrameTester Diamond

    describe "Unit tests" $ do
      it "Splits files" $ do
        splitFileName "costam.png" `shouldBe` ("costam", "png")
        splitFileName "costam.png.jpg" `shouldBe` ("costam", "png.jpg")
        splitFileName "costam.pnadgdgadgag" `shouldBe` ("costam", "pnadgdgadgag")
        splitFileName ".costam.png" `shouldBe` ("", "costam.png")
        splitFileName "..." `shouldBe` ("", "..")

    describe "conversion beetwen image representations (Unit tests)"  $
      it "convert" $ do
        imageRepaTester function1 `shouldBe` True
        imageRepaTester function2 `shouldBe` True
        imageRepaTester function3 `shouldBe` True
          where function1 = fromFunction ( Z :. 0 :. 0 ) (\(Z :. _ :. _) -> (0,0,0,0))
                function2 = fromFunction ( Z :. 10 :. 10 ) (\(Z :. _ :. _) -> (0,0,0,0))
                function3 = fromFunction ( Z :. 1024 :. 2048 ) (\(Z :. x :. y) -> (255,255,255,255))
