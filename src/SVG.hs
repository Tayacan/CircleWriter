{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}
module SVG where

import Data.List (intercalate)

data SvgElement = SvgElement
  { elementName :: String
  , attributes :: [BoxedAttribute]
  } deriving Show

data SvgAttribute a = SvgAttribute
  { attributeName :: String
  , attributeValue :: a
  }

instance AttributeValue a => Show (SvgAttribute a) where
  show (SvgAttribute name val) = name ++ "=\"" ++ showValue val ++ "\""

class AttributeValue a where
  showValue :: a -> String

instance AttributeValue [Char] where
  showValue = id

instance AttributeValue Int where showValue = show
instance AttributeValue Integer where showValue = show
instance AttributeValue Float where showValue = show
instance AttributeValue Double where showValue = show

newtype SpacedList a = SpacedList [a]

instance AttributeValue a => AttributeValue (SpacedList a) where
  showValue (SpacedList xs) = intercalate " " $ map showValue xs

data BoxedAttribute = forall a. AttributeValue a => BA (SvgAttribute a)

instance Show BoxedAttribute where
  show (BA attr) = show attr

class Render a where
  render :: a -> String

instance Render SvgElement where
  render (SvgElement name attrs) = "<" ++ name ++ concatMap ((' ':) . show) attrs ++ " />"

instance Render a => Render [a] where
  render = concatMap render
