module ZendoEval where

import JboProp
import Logic
import Bindful
import Koan
import Sumti
import ZendoParse (Rule)
import Selbri (selbriForRel)
import Error (OrError)
import JboSyntax (Numeral(..), AbsMex(..))

import Control.Monad (foldM)
import Data.Maybe (catMaybes)

-- Tries to parse a Mex numeral list to an integer
evalNumeralString :: [Numeral] -> OrError Int
evalNumeralString ns =
  let calc [] = 0
      calc (d:ds) = (10 * calc ds) + d in
  fmap (calc . fmap (\d -> case d of
    "no" -> 0
    "pa" -> 1
    "re" -> 2
    "ci" -> 3
    "vo" -> 4
    "mu" -> 5
    "xa" -> 6
    "ze" -> 7
    "bi" -> 8
    "so" -> 9
  )) .
  sequence .
  fmap (\x -> case x of
    PA p -> Right p
    NumeralLerfu l -> Left $ "unknown numeral lerfu " ++ show l
  ) $ ns

-- Converts a JboMex to an int predicate
evalMex :: JboMex -> OrError (Int -> Bool)
evalMex (MexInt n) = return $ (==) n
evalMex (MexNumeralString ns) = case ns of
  (PA "su'o" : ns) -> fmap (\n i -> i >= n) $ evalNumeralString ns
  (PA "su'e" : ns) -> fmap (\n i -> i <= n) $ evalNumeralString ns
  (PA "me'i" : ns) -> fmap (\n i -> i < n) $ evalNumeralString ns
  (PA "za'u" : ns) -> fmap (\n i -> i > n) $ evalNumeralString ns
  _ -> Left $ "unknown mex numeral string " ++ show ns
evalMex mex = Left $ "unknown mex expression " ++ show mex

-- Counts number of bools that are true
boolCount :: [Bool] -> Int
boolCount = foldl (\x y -> x + if y then 1 else 0) 0

mapTerms :: [JboTerm] -> Bindful a (OrError [a])
mapTerms ts = do
  stuff <-
    mapM (\t ->
      case t of
        BoundVar n -> fmap Right (binding n)
        _ -> return $ Left ("unknown term " ++ show t)
    ) ts
  return $ sequence stuff

-- Evaluates a quantified statement by substituting in all possible objects from the
-- universe (Koan) and then applying f to the list of Bools.
evalQuant :: Maybe (Int -> JboProp)
          -> (Int -> JboProp)
          -> Koan
          -> ([Bool] -> Bool)
          -> Bindful Sumti (OrError Bool)
evalQuant d p k f =
  let
    any = fmap (fmap f . sequence . catMaybes) . sequence
    sumti = sumtiInKoan k
  in
  any $
    fmap (\sumti ->
      withBinding sumti (\x -> do
        inDomain <- case d of
          Nothing -> return (Right True)
          Just d -> evalProp' (d x) k
        case inDomain of
          Right True -> fmap Just $ evalProp' (p x) k
          Right False -> return Nothing
          Left e -> return $ Just $ Left e
        )
    ) sumti

evalProp :: JboProp -> Koan -> OrError Bool
evalProp p k = evalBindful (evalProp' p k)

evalProp' :: JboProp -> Koan -> Bindful Sumti (OrError Bool)
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
evalProp' (Quantified (LojQuantifier (Exactly n)) d p) k = evalQuant d p k ((== n) . boolCount)
evalProp' (Quantified (MexQuantifier mex) d p) k =
  case evalMex mex of
    Right f -> evalQuant d p k (f . boolCount)
    Left e -> return $ Left e

evalProp' (Rel r ts) k = do
  ts <- mapTerms ts
  return $ do
    ts <- ts
    selbri <- selbriForRel r 
    return $ selbri k ts

evalProp' (NonLogConnected _ _ _) _ = return $ Left "non-logical connectives not supported"
evalProp' (Modal _ _) _ = return $ Left "modals not supported"
evalProp' _ _ = return $ Left "some unknown element"

satisfiesRule :: Rule -> Koan -> OrError Bool
satisfiesRule ps k = fmap (all id) . sequence . fmap (flip evalProp $ k) $ ps
