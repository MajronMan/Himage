{-# LANGUAGE PackageImports, BangPatterns, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-incomplete-patterns #-}
-----
-- functionP is parallel evaluation function
-----
module Filters.Figures
    (
    Point(Point),
    Figure(Circle,Square,Diamond),
    center,
    radius,
    inside,
    insideFrame,
    outside,
    norm,
    enlarge,
    applyPartiallyInFigureP,
    addFrame
    ) where

import Control.Monad
import Data.Array.Repa as Repa hiding ((++))

import Filters.Types

data Point = Point {x::Int, y::Int}

instance Show Point where
  show (Point x y) = "x = " ++ (show x) ++ " y = " ++ (show y)


instance Eq Point where
  (==) (Point x1 y1) (Point x2 y2) = and [x1==x2, y1==y2]

data Figure =
    Circle {center::Point, radius::Double}
  | Square {center::Point, radius::Double} --promień to połowa długości przekątnej
  | Diamond {center::Point, radius::Double} --promień to połowa długości przekątnej

instance Show Figure where
  show (Circle p r) = "Circle, center = " ++ show p ++ ", radius = " ++ show r
  show (Square p r) = "Circle, center = " ++ show p ++ ", radius = " ++ show r
  show (Diamond p r) = "Circle, center = " ++ show p ++ ", radius = " ++ show r

--odległość od środka figury do punktu w odpowiedniej normie
norm:: Figure ->  Point -> Double
norm (Circle c r) p = sqrt (fromIntegral (((x p) - (x c))^2 + ((y p) - (y c))^2))
norm (Square c r) p = fromIntegral (max (abs ((x p) - (x c))) (abs ((y p) - (y c))))
norm (Diamond c r) p = fromIntegral (abs ((x p) - (x c)) + abs ((y p) - (y c)))

inside:: Figure -> Point -> Bool
inside figure point = (norm figure point <= ( radius  figure))

outside:: Figure -> Point -> Bool
outside figure point = not $ inside figure point

insideFrame:: Figure -> Double -> Point -> Bool
insideFrame figure frameWidth point =
  (outside figure point) && (inside (enlarge figure ( frameWidth)) point)

enlarge:: Figure -> Double -> Figure
enlarge (Circle c r) d = (Circle c (r+d))
enlarge (Diamond c r) d = (Diamond c (r+d))
enlarge (Square c r) d = (Square c (r+d))

doubleAbs :: Double -> Double
doubleAbs a
  | a < 0 = -a
  | otherwise = a

--Przyjmuje funkcje inside lub outside i zwraca odpowiednio wnętrze figury lub zewnętrze, reszta czarna i przezroczysta
cutFigure :: Figure -> (Figure->Point->Bool)-> Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
cutFigure figure fun matrix =
  fromFunction
  (extent matrix)
  f
  where
    f = (\(Z :. w :. h) ->  if (fun figure (Point w h))
                            then matrix!(Z :. w :. h)
                            else (0,0,0,0))

--wyodrębnia podaną figurę i aplikuje jeden filtr wewnątrz, a drugi na zewnątrz
applyPartiallyInFigureP :: Figure
                          -> (Array D DIM2 RGBA8->IO(Array D DIM2 RGBA8)) --filtr wewnętrzny
                          -> (Array D DIM2 RGBA8->IO(Array D DIM2 RGBA8)) --filtr zewnętrzny
                          -> Array D DIM2 RGBA8
                          -> IO(Array D DIM2 RGBA8)
applyPartiallyInFigureP figure innerFun outterFun matrix =
  do
  insideFigure <- innerFun matrix
  outsideFigure <- outterFun matrix
  let insideFigure' = cutFigure figure inside insideFigure
  let outsideFigure' = cutFigure figure outside outsideFigure
  return (Repa.zipWith addRGBA8 insideFigure' outsideFigure')

addFrame :: Figure -> Double -> RGBA8 -> Array D DIM2 RGBA8 -> Array D DIM2 RGBA8
addFrame figure frameWidth frameColour matrix =
  fromFunction
  (extent matrix)
  fun
  where
    fun = (\(Z :. w :. h) ->  if insideFrame figure frameWidth (Point w h)
                              then frameColour
                              else matrix!(Z :. w :. h))
