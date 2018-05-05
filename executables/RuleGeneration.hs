module RuleGeneration where

import Data.List (intercalate)
import Data.Set (Set)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import System.Random
import System.Random.Shuffle
import Control.Monad.Random

import Koan
import KoanGeneration
import ZendoParse (tersmu)
import ZendoEval (boolCount, satisfiesRule)
import Error (OrError)

fullSatisfies :: String -> Koan -> OrError Bool
fullSatisfies s k = do
  rule <- tersmu s
  result <- satisfiesRule rule k
  return $ result

pickWeighted :: (RandomGen g) => [(Int, a)] -> Rand g a
pickWeighted xs =
  let sum = foldl ( + ) 0 . map fst $ xs in do
  r <- getRandomR (0, sum)
  return $ loop r xs
    where 
      loop r xs =
        case xs of
          (w, a) : more ->
            if r <= w
              then a
              else loop (r - w) more
          [] -> error "empty weight list"

sentenceBody :: (RandomGen g) => () -> Rand g (String, Set String)
sentenceBody () =
  let da = Set.fromList ["da"]
      dade = Set.fromList ["da", "de"] in
  pickWeighted
    [ (3, ("da blanu", da))
    , (3, ("da crino", da))
    , (3, ("da xunre", da))
    , (3, ("da pelxu", da))
    , (3, ("da barda", da))
    , (3, ("da norbra", da))
    , (3, ("da cmalu", da))
    , (3, ("da sraji", da))
    , (3, ("da pinta", da))
    , (6, ("da pencu de", dade))
    , (4, ("da pencu lo loldi", da))
    , (6, ("da farsni de", dade))
    , (2, ("da gapru de", dade))
    , (2, ("da cnita de", dade))
    , (2, ("da cpana de", dade))
    , (2, ("da skari de", dade))
    , (2, ("da nilbra de", dade))
    , (1, ("da zmadu de lo ka barda", dade))
    , (1, ("da zmadu de lo ka galtu", dade))
    , (1, ("da traji lo ka barda", da))
    , (1, ("da traji lo ka galtu", da))
    ]

qualifyVariable :: (RandomGen g) => String -> Rand g String
qualifyVariable v = do
  front <- pickWeighted
    [ (5, "su'o")
    , (4, "pa")
    , (3, "re")
    , (2, "ci")
    , (3, "no")
    , (4, "ro")
    , (4, "su'o re")
    ]
  back <- pickWeighted
    [ (10, "")
    , (2, " poi blanu ku'o")
    , (2, " poi crino ku'o")
    , (2, " poi xunre ku'o")
    , (2, " poi pelxu ku'o")
    , (2, " poi barda ku'o")
    , (2, " poi norbra ku'o")
    , (2, " poi cmalu ku'o")
    , (2, " poi sraji ku'o")
    , (2, " poi pinta ku'o")
    , (4, " poi farsni da ku'o")
    , (1, " poi pencu da poi pirmidi ku'o ku'o")
    , (1, " poi traji lo ka barda ku'o")
    , (1, " poi traji lo ka galtu ku'o")
    ]
  return $ front ++ " " ++ v ++ back

sentence :: (RandomGen g) => () -> Rand g String
sentence () = do
  (body, vars) <- sentenceBody ()
  shuffledVars <- shuffleM $ Set.toList vars
  prenex <- fmap (intercalate " ") . sequence . map qualifyVariable $ shuffledVars
  return $ prenex ++ " zo'u " ++ body

-- Checks whether the rule is feasible
testRule :: (RandomGen g) => String -> Rand g Bool
testRule rule = do
  koans <- sequence (replicate 100 (randomKoan 3))
  return $ case sequence $ map (fullSatisfies rule) koans of
    Right bools ->
      let count = boolCount bools in 
      count > 5 && count < 95
    Left _ -> False

rule :: (RandomGen g) => () -> Rand g String
rule () = do
  r <- sentence ()
  test <- testRule r
  if test
    then return r
    else rule ()
