{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module Circles
( circle, arc, dot, line, triangle
, Point (..), Circle (..)
, svg
, pointOnCircle
) where

import Data.List (intersperse)
import Text.Printf (printf)
import SVG

data Point = Point { x :: Int, y :: Int } deriving Show
data BoundingBox = BBox { lowerLeft :: Point, upperRight :: Point } deriving Show

data Circle = Circle
  { radius :: Int
  , center :: Point
  } deriving Show

data Arc = Arc
  { baseCircle :: Circle
  , start0Angle :: Float
  , endAngle :: Float
  } deriving Show

data Line = Line
  { from :: Point
  , to :: Point
  } deriving Show

data Triangle = Triangle
  { sideLength :: Int
  , position :: Point
  , angle :: Float
  } deriving Show

newtype Dot = Dot Circle

class Drawable a where
  draw :: a -> SvgElement
  bbox :: a -> BoundingBox

pointOnCircle :: Circle -> Float -> Point
pointOnCircle (Circle r (Point x' y')) a = Point (round x) (round y)
  where x = fromIntegral x' + cos a * fromIntegral r
        y = fromIntegral y' + sin a * fromIntegral r

data DrawableBox = forall a. Drawable a => DB a

instance Drawable Triangle where
  draw (Triangle s (Point x y) a) = SvgElement "path" attrs
    where attrs = [ BA $ SvgAttribute "d" path
                  , BA $ SvgAttribute "stroke" "black"
                  , BA $ SvgAttribute "stroke-width" (2 :: Int)
                  , BA $ SvgAttribute "fill" "white"
                  ]
          path = SpacedList $ "M" : intersperse "L" (map (\(Point x' y') -> show x' ++ " " ++ show y') points) ++ ["Z"]
          points = map (pointOnCircle (Circle (round $ fromIntegral s / sqrt 3) (Point x y))) [a, 2 * pi / 3 + a, 4 * pi / 3 + a]

  bbox (Triangle s (Point x y) a) = getBBox $ map (\p -> BBox p p) points
    where points = map (pointOnCircle (Circle (round $ 2 * height / 3) (Point x y))) [a, 2 * pi / 3 + a, 4 * pi / 3 + a]
          height :: Float
          height = sqrt $ fromIntegral s ** 2 - (fromIntegral s / 2) ** 2

instance Drawable Circle where
  draw (Circle r (Point x y)) = SvgElement "circle" attrs
    where attrs = [ BA $ SvgAttribute "cx" x
                  , BA $ SvgAttribute "cy" y
                  , BA $ SvgAttribute "r" r
                  , BA $ SvgAttribute "stroke" "black"
                  , BA $ SvgAttribute "stroke-width" (2 :: Int)
                  , BA $ SvgAttribute "fill" "white"
                  ]
  bbox (Circle r (Point x y)) = BBox (Point (x - r) (y - r)) (Point (x + r) (y + r))

instance Drawable Arc where
  bbox = bbox . baseCircle
  draw (Arc (Circle r (Point x y)) s e) = SvgElement "path" attrs
    where attrs = [ BA $ SvgAttribute "d" pathAttr
                  , BA $ SvgAttribute "stroke" "black"
                  , BA $ SvgAttribute "stroke-width" (2 :: Int)
                  , BA $ SvgAttribute "fill" "white"
                  ]
          pathAttr = SpacedList
            [ "M", printf "%0.2f" (xpos x s), printf "%0.2f" (ypos y s)
            , "A", show r, show r, "0", bigFlag, "1", endCoords
            ]
          xpos c a = fromIntegral c + cos a * fromIntegral r
          ypos c a = fromIntegral c + sin a * fromIntegral r
          bigFlag = if e - s >= pi || s > e && s - e <= pi then "1" else "0"
          endCoords = printf "%0.2f" (xpos x e) ++ " " ++ printf "%0.2f" (ypos y e)

instance Drawable Dot where
  draw (Dot c) = let (SvgElement n attrs) = draw c
                 in SvgElement n $ (BA $ SvgAttribute "fill" "black") : init attrs
  bbox (Dot c) = bbox c

instance Drawable Line where
  draw (Line (Point x1 y1) (Point x2 y2)) = SvgElement "line" attrs
    where attrs = [ BA $ SvgAttribute "x1" x1
                  , BA $ SvgAttribute "x2" x2
                  , BA $ SvgAttribute "y1" y1
                  , BA $ SvgAttribute "y2" y2
                  , BA $ SvgAttribute "stroke" "black"
                  , BA $ SvgAttribute "stroke-width" (2 :: Int)
                  ]
  bbox (Line (Point x1 y1) (Point x2 y2)) = BBox (Point (min x1 x2) (min y1 y2)) (Point (max x1 x2) (max y1 y2))

instance Drawable DrawableBox where
  draw (DB a) = draw a
  bbox (DB a) = bbox a

getBBox :: [BoundingBox] -> BoundingBox
getBBox [] = BBox (Point 0 0) (Point 0 0)
getBBox (d:ds) = foldl (\acc d -> extendBox acc d) d ds
  where extendBox (BBox a b) (BBox c d) = BBox (Point (min (x a) (x c)) (min (y a) (y c))) (Point (max (x b) (x d)) (max (y b) (y d)))

triangle :: Int -> Point -> Float -> DrawableBox
triangle s p a = DB $ Triangle s p a

circle :: Int -> Point -> DrawableBox
circle r p = DB $ Circle r p

dot :: Int -> Point -> DrawableBox
dot r p = DB $ Dot $ Circle r p

arc :: Int -> Point -> Float -> Float -> DrawableBox
arc r p start end = DB $ Arc (Circle r p) start end

line :: Point -> Point -> DrawableBox
line p1 p2 = DB $ Line p1 p2

svg :: Drawable a => [a] -> String
svg xs = svgStart ++ render (map draw xs) ++ svgEnd
  where svgStart = "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\""  ++ show height
                                                       ++ "\" width=\""   ++ show width
                                                       ++ "\" viewBox=\"" ++ show left ++ " "
                                                                          ++ show bot ++ " "
                                                                          ++ show width ++ " "
                                                                          ++ show height ++ "\" >"
        svgEnd   = "</svg>"
        width = right - left
        height = top - bot
        (BBox (Point left' bot') (Point right' top')) = getBBox $ map bbox xs
        (left, bot, right, top) = (left' - padding, bot' - padding, right' + padding, top' + padding)
        padding = 5
