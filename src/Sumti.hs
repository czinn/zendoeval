module Sumti where

import Koan

data Sumti =
    Pyramid Int Int -- index of KoanPart, index in stack
  | Spot Int Int    -- spot on pyramid at corresponding indexes
  | Column Int      -- index of KoanPart. Use only for Stacks
  | ConcreteColour Colour
  | ConcreteSize Size
  | Ground
  | Property (Koan -> Sumti -> Int)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

pyramidWithSpots :: Koan.Pyramid -> Int -> Int -> [Sumti]
pyramidWithSpots (Koan.Pyramid z _) i j =
  (Sumti.Pyramid i j) : replicate (fromEnum z + 1) (Spot i j)

sumtiInKoan :: Koan -> [Sumti]
sumtiInKoan k =
  Ground :
  fmap ConcreteColour [Blue, Green, Red, Yellow]
  ++
  fmap ConcreteSize [Small, Medium, Large]
  ++
  do
    (i, part) <- enumerate k
    case part of
      Stack pyramids -> Column i : do
        (j, pyramid) <- enumerate pyramids
        pyramidWithSpots pyramid i j
      Pointing _ pyramid -> pyramidWithSpots pyramid i 0
      Empty -> []
