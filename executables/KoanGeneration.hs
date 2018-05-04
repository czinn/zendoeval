module KoanGeneration where

import System.Random
import Control.Monad.Random

import Koan
import Error (OrError)

addPyramid :: Koan -> [Koan]
addPyramid k = do
  size <- [Small, Medium, Large]
  colour <- [Blue, Green, Red, Yellow]
  let pyramid = Pyramid size colour
      newParts = [Stack [pyramid], Pointing Lft pyramid, Pointing Rgt pyramid]
      in
    case k of
      (Stack ps : xs) -> [ Stack (pyramid : ps) : xs ]
      _ -> []
    ++
    case k of
      [] -> []
      _ -> fmap (\newPart -> newPart : Empty : k) newParts 
    ++
    fmap (\newPart -> newPart : k) newParts

-- generateKoans n generates all koans with n or fewer pyramids.
generateKoans :: Int -> [Koan]
generateKoans 0 = [[]]
generateKoans n =
  let base = generateKoans (n - 1) in
  base ++ do
    koan <- base
    addPyramid koan

randomKoan :: RandomGen g => Int -> Rand g Koan
randomKoan 0 = return []
randomKoan n = do
  k <- randomKoan (n - 1)
  let koans = addPyramid k in do
    index <- getRandomR (0, length koans - 1)
    return $ koans !! index

-- Returns Nothing if a counterexample could not be found. Returns an error if
-- either of the rules produced an error while evaluating koans.
counterexample :: RandomGen g => (Koan -> OrError Bool) -> (Koan -> OrError Bool) -> Rand g (OrError (Maybe Koan))
counterexample a b =
  let sizes = replicate 10000 2 ++ replicate 10000 3 ++ replicate 5000 4 ++ replicate 2500 5 in
  loop sizes
  where loop [] = return $ Right Nothing
        loop (n:ns) = do
          k <- randomKoan n
          case (do
            evalA <- a k
            evalB <- b k
            return $ evalA == evalB) of
            Right False -> return $ Right $ Just k
            Right True -> loop ns
            Left e -> return $ Left e
