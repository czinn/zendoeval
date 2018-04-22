module Koan where

data Colour = Blue | Green | Red | Yellow deriving (Eq, Show, Enum)
data Size = Small | Medium | Large deriving (Eq, Show, Enum)
data Pyramid = Pyramid Size Colour deriving (Eq, Show)
data Direction = Left | Right deriving (Eq, Show)
data KoanPart = Stack [Pyramid]
              | Pointing Direction Pyramid
              | Empty deriving (Eq, Show)
type Koan = [KoanPart]
