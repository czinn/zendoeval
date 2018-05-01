module Main where

import ZendoParse (tersmu)
import ZendoEval (satisfiesRule)
import Koan

koan = [Stack [Pyramid Large Red, Pyramid Small Blue]]

main :: IO ()
main = do
  s <- getLine
  case tersmu s of
    Left e -> putStrLn $ "Error: " ++ e
    Right rule -> do
      case satisfiesRule rule koan of
        Right True -> putStrLn "yes"
        Right False -> putStrLn "no"
        Left e -> putStrLn $ "Error: " ++ e
