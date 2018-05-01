module ZendoParse where

import ParseText (parseText)
import JboProp (JboProp, propTexticules)
import Morph (morph)
import ParseM (evalParseStateT)
import JboParse (evalText)
import Error (OrError)

import Control.Monad.State
import Control.Monad.Identity

type Rule = [JboProp]

tersmu :: String -> OrError Rule
tersmu s =
  case morph s of
  Left errpos -> Left $ "morph error at " ++ show errpos
  Right text ->
    case parseText text of
    Left errpos -> Left $ "morph error at " ++ show errpos
    Right text -> Right ((propTexticules . runIdentity . evalParseStateT . evalText) text)
