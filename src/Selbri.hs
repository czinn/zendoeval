module Selbri where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace

import JboProp (JboRel(..))

import Koan
import Sumti

type Selbri = Koan -> [Sumti] -> Bool

nth :: [a] -> Int -> Maybe a
nth (x:_) 0 = Just x
nth (_:xs) n = nth xs (n - 1)
nth [] _ = Nothing

getPyramid :: Koan -> Sumti -> Maybe Koan.Pyramid
getPyramid parts (Sumti.Pyramid i j) = do
  part <- nth parts i
  case part of
    Stack pyramids -> nth pyramids j
    Pointing _ p -> return p
    Empty -> Nothing
-- getPyramid _ _ = Nothing

colourSelbri :: Colour -> Selbri
colourSelbri c k [s] =
    case getPyramid k s of
      Just (Koan.Pyramid _ c') -> c == c'
      Nothing -> False
colourSelbri _ _ _ = False

sizeSelbri :: Size -> Selbri
sizeSelbri z k [s] =
  case getPyramid k s of
    Just (Koan.Pyramid z' _) -> z == z'
    Nothing -> False
sizeSelbri _ _ _ = False

blanu = colourSelbri Blue
crino = colourSelbri Green
xunre = colourSelbri Red
pelxu = colourSelbri Yellow

barda = sizeSelbri Large
norbra = sizeSelbri Medium
cmalu = sizeSelbri Small

pirmidi _ [Sumti.Pyramid _ _] = True
pirmidi _ _ = False

sraji k [Sumti.Pyramid i _] = case nth k i of
  Just (Stack _) -> True
  _ -> False 
-- sraji _ _ = False
pinta k [Sumti.Pyramid i _] = case nth k i of
  Just (Pointing _ _) -> True
  _ -> False 
-- pinta _ _ = False

-------------------------------------------------
-- Touching logic; very prone to weird edge cases

-- Produces the height of its tip above the ground for each pyramid in the
-- stack.
pyramidHeights :: KoanPart -> [Int]
pyramidHeights (Pointing _ (Koan.Pyramid z _)) =
  [fromEnum z + 1]
pyramidHeights (Stack ps) =
  tail
  . scanl (\h (Koan.Pyramid z _) ->
    let z' = fromEnum z + 2 in
    max (h + 1) z'
  ) 0
  $ ps
pyramidHeights Empty = []

-- For each unit height above the ground (starting from 0, on the ground),
-- gives the width of the part at that height and the corresponding pyramid
-- index attaining that width.
widthProfile :: KoanPart -> Direction -> [(Int, Int)]
widthProfile (Pointing d (Koan.Pyramid z _)) d' =
  if d == d'
    then [(4, 0)]
    else map (\x -> (x, 0)) $ takeWhile (> 0) $ iterate pred $ fromEnum z + 2
widthProfile (Stack ps) _ =
  let heights = pyramidHeights (Stack ps) in
  foldl (\profile (h, (j, (Koan.Pyramid z _))) ->
    let b = h - (fromEnum z + 2) in
    merge b profile $ map (\x -> (x, j)) $ takeWhile (> 0) $ iterate pred $ fromEnum z + 2
  ) [] $ zip heights (zip [0..] ps)
  where zipDefault xs ys = takeWhile (\((x, _), (y, _)) -> x > 0 || y > 0)
                           $ zip (xs ++ repeat (0, 0)) (ys ++ repeat (0, 0))
        merge n xs ys = map (\(x, y) -> max x y) $ zipDefault xs (replicate n (0, 0) ++ ys)

touchingPairs :: KoanPart -> KoanPart -> [(Int, Int)]
touchingPairs x y =
  let p1 = widthProfile x Koan.Right
      p2 = widthProfile y Koan.Left in
  snd $ foldl (\(mw, ps) ((w1, j1), (w2, j2)) ->
    let nw = w1 + w2 in
    if nw > mw
      then (nw, [(j1, j2)])
      else if nw == mw
        then (mw, (j1, j2) : ps)
        else (mw, ps)
  ) (0, []) $ zip p1 p2

pencu :: Koan -> [Sumti] -> Bool
pencu k [Sumti.Pyramid i1 j1, Sumti.Pyramid i2 j2] =
  if i1 > i2
    then pencu k [Sumti.Pyramid i2 j2, Sumti.Pyramid i1 j1]
    else if i2 - i1 >= 2 || (i1 == i2 && abs (j1 - j2) >= 2)
      then False
      else if i1 == i2
        then
          let heights = pyramidHeights (fromJust $ nth k i1)
              h1 = fromJust $ nth heights j1
              h2 = fromJust $ nth heights j2 in
          abs (h1 - h2) == 1
        else elem (j1, j2) $ touchingPairs (fromJust $ nth k i1) (fromJust $ nth k i2)
pencu _ _ = False

touchesGround :: Koan -> Sumti -> Bool
touchesGround k (Sumti.Pyramid i j) =
  fromMaybe False (do
    part <- nth k i
    case part of
      Pointing _ _ -> return True
      Stack ps ->
        let heights = pyramidHeights (Stack ps) in
        do
          Koan.Pyramid z _ <- getPyramid k (Sumti.Pyramid i j)
          height <- nth heights j
          return (fromEnum z + 2 == height)
    )

farsni :: Koan -> [Sumti] -> Bool
farsni k [Sumti.Pyramid i1 j1, Sumti.Pyramid i2 j2] =
  if i1 == i2
    then j1 < j2
    else
      touchesGround k (Sumti.Pyramid i2 j2) &&
      fromMaybe False (do
        part <- nth k i1
        return $ case part of
          Stack _ -> False
          Pointing Koan.Left _ -> i1 > i2
          Pointing Koan.Right _ -> i1 < i2
          _ -> False
      )

  -- Map from relations to selbri
selbriForRel :: JboRel -> Maybe Selbri
selbriForRel (Brivla "blanu") = Just blanu
selbriForRel (Brivla "crino") = Just crino
selbriForRel (Brivla "xunre") = Just xunre
selbriForRel (Brivla "pelxu") = Just pelxu
selbriForRel (Brivla "barda") = Just barda
selbriForRel (Brivla "norbra") = Just norbra
selbriForRel (Brivla "cmalu") = Just cmalu
selbriForRel (Brivla "pirmidi") = Just pirmidi
selbriForRel (Brivla "sraji") = Just sraji
selbriForRel (Brivla "pinta") = Just pinta
selbriForRel (Brivla "pencu") = Just pencu
selbriForRel (Brivla "farsni") = Just farsni
selbriForRel _ = Nothing
