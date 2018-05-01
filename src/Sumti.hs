module Sumti where

import Koan

data Sumti =
    Pyramid Int Int -- index of KoanPart, index in stack
  | Column Int      -- index of KoanPart. Use only for Stacks
  | ConcreteColour Colour
  | ConcreteSize Size
  deriving (Show)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

sumtiInKoan :: Koan -> [Sumti]
sumtiInKoan k =
  fmap ConcreteColour [Blue, Green, Red, Yellow]
  ++
  fmap ConcreteSize [Small, Medium, Large]
  ++
  do
    (i, part) <- enumerate k
    case part of
      Stack pyramids -> Column i : do
        (j, pyramid) <- enumerate pyramids
        return $ Sumti.Pyramid i j
      Pointing _ _ -> return $ Sumti.Pyramid i 0
      Empty -> []
