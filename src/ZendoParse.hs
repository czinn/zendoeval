module ZendoParse where

import ParseText (parseText)
import JboProp (JboProp, propTexticules)
import Morph (morph)
import ParseM (evalParseStateT)
import JboParse (evalText)

import Control.Monad.State
import Control.Monad.Identity

type Rule = [JboProp]

tersmu :: String -> Maybe Rule
tersmu s =
  case morph s of
  Left errpos -> Nothing
  Right text ->
    case parseText text of
    Left errpos -> Nothing
    Right text -> Just ((propTexticules . runIdentity . evalParseStateT . evalText) text)
