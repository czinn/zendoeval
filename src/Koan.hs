{-# LANGUAGE DeriveGeneric #-}

module Koan where

import GHC.Generics

data Colour = Blue | Green | Red | Yellow deriving (Eq, Show, Read, Generic, Enum)
data Size = Small | Medium | Large deriving (Eq, Show, Read, Generic, Enum)
data Pyramid = Pyramid Size Colour deriving (Eq, Show, Read, Generic)
data Direction = Lft | Rgt deriving (Eq, Show, Read, Generic)
data KoanPart = Stack [Pyramid]
              | Pointing Direction Pyramid
              | Empty deriving (Eq, Show, Read, Generic)
type Koan = [KoanPart]
