module Sumti where

import Koan

data Sumti =
  Pyramid Int Int -- index of KoanPart, index in stack
  deriving (Show)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

sumtiInKoan :: Koan -> [Sumti]
sumtiInKoan k = do
  (i, part) <- enumerate k
  case part of
    Stack pyramids -> do
      (j, pyramid) <- enumerate pyramids
      return $ Sumti.Pyramid i j
    Pointing _ _ -> return $ Sumti.Pyramid i 0
    Empty -> []
