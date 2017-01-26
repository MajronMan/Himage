module Main
where
  import Lib
  import Test.Hspec
  import Test.QuickCheck
  import Data.Typeable
  import Data.Array.Repa
  import Codec.Picture
  import Filters.Figures

  type Arr = Array D DIM2 Int
  type ArrVis = Array U DIM2 Int

  conv4 :: (Arr, Arr, Arr, Arr) -> (ArrVis, ArrVis, ArrVis, ArrVis)
  conv4 (a,b,c,d) = (computeUnboxedS a, computeUnboxedS b, computeUnboxedS c, computeUnboxedS d)

  ziptest :: Arr -> Arr -> Arr -> Arr -> Array D DIM2 (Int,Int,Int,Int)
  ziptest a b c d = zip4 a b c d

  unziptest :: Array D DIM2 (Int,Int,Int,Int) -> (Arr, Arr, Arr, Arr)
  unziptest a = unzip4 a

  compare4 :: (ArrVis, ArrVis, ArrVis, ArrVis) -> (ArrVis, ArrVis, ArrVis, ArrVis) -> Bool
  compare4 (a1, b1, c1, d1) (a2, b2, c2, d2) = and[equalsS a1 a2, equalsS b1 b2, equalsS c1 c2, equalsS d1 d2]

  tester :: (ArrVis, ArrVis, ArrVis, ArrVis) -> Bool
  tester (a1, a2, a3, a4) = compare4 res1 res2 where
    res1 = (a1, a2, a3, a4)
    res2 = conv4(unziptest (ziptest (delay a1) (delay a2) (delay a3) (delay a4)))



  testerCircle :: Int ->Int ->Double ->Int  -> Int -> Bool
  testerCircle a b c d e  =  (norm figure point == 0) == (center figure  == point)
    where figure = (Circle (Point a b) c )
          point = (Point d e)

  testerSquare :: Int ->Int ->Double ->Int  -> Int -> Bool
  testerSquare a b c d e  =  (norm figure point == 0) == (center figure  == point)
    where figure = (Square (Point a b) c )
          point = (Point d e)

  testerDiamond :: Int ->Int ->Double ->Int  -> Int -> Bool
  testerDiamond a b c d e  =  (norm figure point == 0) == (center figure  == point)
    where figure = (Diamond (Point a b) c )
          point = (Point d e)

  --instance Arbitrary Point where
    --arbitrary = (Point (arbitrary x) (arbitrary y))

--  instance Arbitrary Figure where
  --  arbitrary = (Circle (Point (arbitrary x) (arbitrary y)) (arbitrary r))

  main :: IO ()
  main = hspec $ do
    describe "|p-q| = 0 <=> p=q" $ do
      it "test for circles" . property $
        testerCircle
      it "test for squares" . property $
        testerSquare
      it "test for diamonds" . property $
        testerDiamond
