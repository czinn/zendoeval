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

-- A list of koans to check for counterexamples
counterexampleList :: RandomGen g => [Rand g Koan]
counterexampleList =
  let sizes = replicate 5000 2 ++ replicate 4000 3 ++ replicate 2000 4 ++ replicate 1000 5 ++ replicate 100 6 in
  map randomKoan sizes

-- Searchs the list for a counterexmaple. Returns Nothing if a counterexample
-- could not be found. Returns an error if either of the rules produced an
-- error while evaluating koans.
counterexampleFromList :: RandomGen g =>
    [Rand g Koan] ->
    (Koan -> OrError Bool) ->
    (Koan -> OrError Bool) ->
    Rand g (OrError (Maybe Koan))
counterexampleFromList list a b =
  loop list
  where loop [] = return $ Right Nothing
        loop (k:ks) = do
          k <- k
          case (do
            evalA <- a k
            evalB <- b k
            return $ evalA == evalB) of
            Right False -> return $ Right $ Just k
            Right True -> loop ks
            Left e -> return $ Left e

-- Searches a sequence of random counterexamples.
counterexample :: RandomGen g =>
    (Koan -> OrError Bool) ->
    (Koan -> OrError Bool) ->
    Rand g (OrError (Maybe Koan))
counterexample = counterexampleFromList counterexampleList
