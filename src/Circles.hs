{-# LANGUAGE ExistentialQuantification #-}
module Circles where

import Data.List (isPrefixOf)
import Text.Printf (printf)

data Point = Point { x :: Int, y :: Int } deriving Show
data BoundingBox = BBox { lowerLeft :: Point, upperRight :: Point } deriving Show
data Circle = Circle { radius :: Int, center :: Point } deriving Show
data Arc = Arc { baseCircle :: Circle, start0Angle :: Float, endAngle :: Float } deriving Show
data Line = Line { from :: Point, to :: Point } deriving Show
newtype Dot = Dot Circle

type SVG = String

concatSvg :: SVG -> SVG -> SVG
concatSvg = (++)

class Drawable a where
  draw :: a -> SVG
  bbox :: a -> BoundingBox

data DrawableBox = forall a. Drawable a => DB a

instance Drawable Circle where
  draw (Circle r (Point x y)) = "<circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"" ++ show r ++ "\" stroke=\"black\" stroke-width=\"2\" fill=\"white\" />"
  bbox (Circle r (Point x y)) = BBox (Point (x - r) (y - r)) (Point (x + r) (y + r))

instance Drawable Arc where
  bbox = bbox . baseCircle
  draw (Arc circ s e) = "<path d=\"" ++ moveTo ++ " A " ++ rs ++ " 0 " ++ bigFlag ++ " 1 " ++ endCoords ++ "\" stroke=\"black\" stroke-width=\"2\" fill=\"white\" />"
    where xpos c a = fromIntegral c + cos a * fromIntegral r
          ypos c a = fromIntegral c + sin a * fromIntegral r
          r = fromIntegral $ radius circ
          moveTo = "M " ++ printf "%0.2f" (xpos (x $ center circ) s) ++ " " ++ printf "%0.2f" (ypos (y $ center circ) s) 
          rs = show r ++ " " ++ show r
          bigFlag = if abs (e - s) >= 180 then "1" else "0"
          endCoords = printf "%0.2f" (xpos (x $ center circ) e) ++ " " ++ printf "%0.2f" (ypos (y $ center circ) e)

instance Drawable Dot where
  draw (Dot c) = replace "white" "black" $ draw c
    where replace _ _ []       = []
          replace [] _ s       = s -- we don't replace the empty string
          replace x y s@(c:cs) = if x `isPrefixOf` s
                                 then y ++ drop (length x) s
                                 else c : replace x y cs
  bbox (Dot c) = bbox c

instance Drawable Line where
  draw (Line (Point x1 y1) (Point x2 y2)) = "<line x1=\"" ++ show x1 ++ "\" x2=\"" ++ show x2 ++ "\" y1=\"" ++ show y1 ++ "\" y2=\"" ++ show y2 ++ "\" stroke=\"black\" stroke-width=\"2\" />"
  bbox (Line (Point x1 y1) (Point x2 y2)) = BBox (Point (min x1 x2) (min y1 y2)) (Point (max x1 x2) (max y1 y2))

instance Drawable DrawableBox where
  draw (DB a) = draw a
  bbox (DB a) = bbox a

instance Drawable a => Drawable [a] where
  draw = foldl (\acc d -> concatSvg acc (draw d)) ""
  bbox []     = BBox (Point 0 0) (Point 0 0)
  bbox (d:ds) = foldl (\acc d -> extendBox acc d) (bbox d) (map bbox ds)
    where extendBox (BBox a b) (BBox c d) = BBox (Point (min (x a) (x c)) (min (y a) (y c))) (Point (max (x b) (x d)) (max (y b) (y d)))

circle :: Int -> Point -> DrawableBox
circle r p = DB $ Circle r p

dot :: Int -> Point -> DrawableBox
dot r p = DB $ Dot $ Circle r p

arc :: Int -> Point -> Float -> Float -> DrawableBox
arc r p start end = DB $ Arc (Circle r p) start end

line :: Point -> Point -> DrawableBox
line p1 p2 = DB $ Line p1 p2

svg :: Drawable a => a -> SVG
svg x = svgStart `concatSvg` draw x `concatSvg` svgEnd
  where svgStart = "<svg xmlns=\"http://www.w3.org/2000/svg\" height=\"" ++ show height ++ "\" width=\"" ++ show width ++ "\" viewBox=\"" ++ show left ++ " " ++ show bot ++ " " ++ show width ++ " " ++ show height ++ "\" >"
        svgEnd   = "</svg>"
        width = right - left
        height = top - bot
        (BBox (Point left' bot') (Point right' top')) = bbox x
        (left, bot, right, top) = (left' - padding, bot' - padding, right' + padding, top' + padding)
        padding = 5
