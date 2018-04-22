module ZendoEval where

import JboProp
import Logic
import Bindful
import Koan
import Sumti
import ZendoParse (Rule)
import Selbri (selbriForRel)

import Control.Monad (foldM)

mapTerms :: [JboTerm] -> Bindful a (Maybe [a])
mapTerms ts = do
  stuff <-
    mapM (\t ->
      case t of
        BoundVar n -> fmap Just (binding n)
        _ -> return Nothing
    ) ts
  return $ sequence stuff

-- Evaluates a quantified statement by substituting in all possible objects from the
-- universe (Koan) and then applying f to the list of Bools.
evalQuant :: Maybe (Int -> JboProp)
          -> (Int -> JboProp)
          -> Koan
          -> ([Bool] -> Bool)
          -> Bindful Sumti (Maybe Bool)
evalQuant d p k f =
  let
    any = fmap (fmap f . sequence) . sequence
    sumti = sumtiInKoan k
    results = 
      fmap (\sumti ->
            withBinding sumti (\x -> do
              inDomain <- case d of
                Nothing -> return (Just True)
                Just d -> evalProp' (d x) k
              case inDomain of
                Just True -> evalProp' (p x) k
                _ -> return (Just False)
              )
            ) sumti
  in
  any results

evalProp :: JboProp -> Koan -> Maybe Bool
evalProp p k = evalBindful (evalProp' p k)

evalProp' :: JboProp -> Koan -> Bindful Sumti (Maybe Bool)
evalProp' (Not p) k = do
  a <- evalProp' p k
  return $ not <$> a

evalProp' (Connected c p q) k = do
  a <- evalProp' p k
  b <- evalProp' q k
  return $ do
    a <- a
    b <- b
    return $ (case c of
      And -> ( && )
      Or -> ( || )
      Impl -> (\x y -> (not x) || y)
      Equiv -> ( == )
      ) a b

evalProp' (Quantified (LojQuantifier Exists) d p) k = evalQuant d p k (any id)
evalProp' (Quantified (LojQuantifier Forall) d p) k = evalQuant d p k (all id)
evalProp' (Quantified (LojQuantifier (Exactly n)) d p) k =
  evalQuant d p k ((== n) . foldl (\x y -> x + if y then 1 else 0) 0)

evalProp' (Rel r ts) k = do
  ts <- mapTerms ts
  return $ do
    ts <- ts
    selbri <- selbriForRel r 
    return $ selbri k ts

evalProp' (NonLogConnected _ _ _) _ = return Nothing
evalProp' (Modal _ _) _ = return Nothing
evalProp' _ _ = return Nothing

satisfiesRule :: Rule -> Koan -> Maybe Bool
satisfiesRule [p] k = evalProp p k
