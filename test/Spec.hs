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
      it "circle norm" . property $
        normTester Circle
      it "square norm" . property $
        normTester Square
      it "diamond norm" . property $
        normTester Diamond

    describe "(!inside => ouotside) and (!outside => inside)" $ do
      it "inside/outside circle" . property $
        insideOutsideTester Circle
      it "inside/outside square" . property $
        insideOutsideTester Square
      it "inside/outside diamond" . property  $
        insideOutsideTester Diamond

    describe "point can't be inside frame with width=0 " $ do
      it "inside circle noframe" . property $
        noFrameTester Circle
      it "inside squares noframe" . property $
        noFrameTester Square
      it "inside diamonds noframe" . property  $
        noFrameTester Diamond

    describe "conversion to and from image (Unit test)"  $
      it "convert" $ do
        imageRepaTester function1 `shouldBe` True
        imageRepaTester function2 `shouldBe` True
        imageRepaTester function3 `shouldBe` True
          where function1 = fromFunction ( Z :. 0 :. 0 ) (\(Z :. _ :. _) -> (0,0,0,0))
                function2 = fromFunction ( Z :. 10 :. 10 ) (\(Z :. _ :. _) -> (0,0,0,0))
                function3 = fromFunction ( Z :. 1024 :. 2048 ) (\(Z :. x :. y) -> (255,255,255,255))
