module Main where

import ZendoParse (tersmu)
import ZendoEval (satisfiesRule)
import Koan

koan = [Stack [Pyramid Large Red, Pyramid Small Blue]]

main :: IO ()
main = do
  s <- getLine
  case tersmu s of
    Nothing -> putStrLn "error" 
    Just rule -> do
      putStrLn (show rule)
      case satisfiesRule rule koan of
        Just True -> putStrLn "yes"
        Just False -> putStrLn "no"
        Nothing -> putStrLn "ask again later"
