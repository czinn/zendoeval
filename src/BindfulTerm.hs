{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,TypeSynonymInstances,FlexibleInstances,AllowAmbiguousTypes #-}

module BindfulTerm where

import JboProp (JboTerm(..))

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type BindfulTerm a = State (Map JboTerm a)

bind :: JboTerm -> a -> BindfulTerm a ()
bind k v = do
  bs <- get
  put $ Map.insert k v bs

withBoundVarBinding :: a -> (Int -> BindfulTerm a r) -> BindfulTerm a r
withBoundVarBinding v f = do
  n <- nextFreeBoundVar
  bind (BoundVar n) v
  r <- f n
  unbind (BoundVar n)
  return r

binding :: JboTerm -> BindfulTerm a (Maybe a)
binding k = do
  bs <- get
  return $ Map.lookup k bs

getValues :: BindfulTerm a [a]
getValues = gets Map.elems

evalBindful :: BindfulTerm a r -> r
evalBindful m = evalState m Map.empty

nextFreeBoundVar :: BindfulTerm a Int
nextFreeBoundVar = do
  bs <- get
  return $ head [ n |
    n <- [1..], isNothing $ Map.lookup (BoundVar n) bs ]

unbind :: JboTerm -> BindfulTerm a ()
unbind k = do
  bs <- get
  put $ Map.delete k bs
