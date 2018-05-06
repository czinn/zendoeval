module Main where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import System.Random
import Control.Monad.Random

import ZendoParse (tersmu, Rule)
import ZendoEval (satisfiesRule)
import Koan
import KoanGeneration
import RuleGeneration
import Error

showPyramid (Pyramid z c) =
  case c of
    Blue -> "b"
    Green -> "g"
    Red -> "r"
    Yellow -> "y"
  ++
  case z of
    Small -> "1"
    Medium -> "2"
    Large -> "3"

showPart part = case part of
  Stack ps -> foldr (\p s -> showPyramid p ++ s) "" ps
  Pointing Lft p -> "<" ++ showPyramid p
  Pointing Rgt p -> showPyramid p ++ ">"
  Empty -> "|"

showKoan = intercalate " " . map showPart

parseKoan = sequence . map parsePart . words

parsePart part = case part of
  ['<', _, _] -> fmap (Pointing Lft) (parsePyramid $ tail part)
  [_, _, '>'] -> fmap (Pointing Rgt) (parsePyramid $ part)
  "|" -> Just Empty
  _ -> fmap Stack $ parsePyramidList part
  where parsePyramidList "" = Just []
        parsePyramidList s =
          let (p, r) = splitAt 2 s in do
            x <- parsePyramid p
            xs <- parsePyramidList r
            return $ x : xs

parsePyramid :: String -> Maybe Pyramid
parsePyramid p = case p of
  (c:z:_) -> do
    colour <- case c of
      'b' -> Just Blue
      'g' -> Just Green
      'r' -> Just Red
      'y' -> Just Yellow
      _ -> Nothing
    size <- case z of
      '1' -> Just Small
      '2' -> Just Medium
      '3' -> Just Large
      _ -> Nothing
    return $ Pyramid size colour

split1 :: Char -> String -> (String, String)
split1 c s = (takeWhile ((/=) c) s, tail . dropWhile ((/=) c) $ s)

doGame :: String -> Rule -> [(Koan, Bool)] -> IO ()
doGame ruleString rule koans =
  doRound koans
  where
    evalKoanWithRule koan = (koan, fromOrError $ satisfiesRule rule koan)
    doRound :: [(Koan, Bool)] -> IO ()
    doRound koans = do
      mapM_ (\(k, b) -> putStrLn $ show b ++ "\t" ++ showKoan k) koans
      s <- getLine
      let (command, arg) = split1 ' ' s in
        case command of
          "guess" ->
            case tersmu arg of
              Right theirRule -> do
                -- Try to find a counterexample in the existing koans first
                ce <- evalRandIO $ counterexampleFromList (map (return . fst) koans) (satisfiesRule rule) (satisfiesRule theirRule)
                case ce of
                  Right (Just koan) -> notify $ "This koan already disproves that rule: " ++ showKoan koan
                  Right (Nothing) -> do
                    -- Try to find a new counterexample
                    ce <- evalRandIO $ counterexample (satisfiesRule rule) (satisfiesRule theirRule)
                    case ce of
                      Right (Just koan) -> doRound $ (evalKoanWithRule koan) : koans
                      Right (Nothing) -> do
                        putStrLn "That looks like my rule!"
                        putStrLn $ "My rule was: " ++ ruleString
                      Left e -> notify $ "Error evaluating rule: " ++ e
                  Left e -> notify $ "Error evaluating rule: " ++ e
              Left e -> notify $ "Error parsing rule: " ++ e
          "build" ->
            case parseKoan arg of
              Just koan -> doRound $ (evalKoanWithRule koan) : koans
              Nothing -> notify "Error parsing koan"
          "end" -> putStrLn $ "My rule was: " ++ ruleString
          _ -> notify "Try 'build' or 'guess' or 'end'"
      where notify :: String -> IO ()
            notify s = do
              putStrLn s
              doRound koans

main :: IO ()
main = do
  ruleString <- evalRandIO $ rule ()
  let rule = fromOrError $ tersmu ruleString
      forceCounterexample f = fmap (fromJust . fromOrError) $ evalRandIO $ counterexample (satisfiesRule rule) f in do
    positiveExample <- forceCounterexample (\_ -> Right False)
    negativeExample <- forceCounterexample (\_ -> Right True)
    -- print ruleString
    doGame ruleString rule [(positiveExample, True), (negativeExample, False)]
