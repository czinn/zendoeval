{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,TypeSynonymInstances,FlexibleInstances,AllowAmbiguousTypes #-}

module BindfulTerm where

import JboProp (JboTerm)

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

class Monad m => BindfulMonad a m | m -> a where
    withBinding :: [JboTerm] -> a -> (JboTerm -> m r) -> m r
    binding :: JboTerm -> m (Maybe a)
    getValues :: m [a]
    evalBindful :: m r -> r

    -- intended to be private:
    nextFree :: [JboTerm] -> m JboTerm
    bind :: JboTerm -> a -> m ()
    unbind :: JboTerm -> m ()
    bindNext :: [JboTerm] -> a -> m ()

type BindfulTerm a = State (Map JboTerm a)

instance BindfulMonad a (BindfulTerm a) where
  withBinding ks v f = do
    k <- nextFree ks
    bind k v
    r <- f k
    unbind k
    return r
  binding k = do
    bs <- get
    return $ Map.lookup k bs
  getValues = gets Map.elems
  evalBindful m = evalState m Map.empty

  nextFree ks = do
    bs <- get
    return $ head [ k |
      k <- ks, isNothing $ Map.lookup k bs ]
  bind k v = do
    bs <- get
    put $ Map.insert k v bs
  unbind k = do
    bs <- get
    put $ Map.delete k bs
  bindNext ks v = do
    k <- nextFree ks
    bind k v
