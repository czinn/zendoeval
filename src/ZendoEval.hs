module ZendoEval where

import JboProp
import Logic
import BindfulTerm
import Koan
import Sumti
import ZendoParse (Rule)
import Selbri (selbriForRel)
import FixConstant (fixConstant)
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

mapTerms :: [JboTerm] -> BindfulTerm a (OrError [a])
mapTerms ts =
  fmap sequence .
  mapM (\t -> do
    bound <- binding t
    return $ case bound of
      Just b -> Right b
      Nothing -> Left ("unbound term " ++ show t)
  ) $ ts

-- Evaluates a quantified statement by substituting in all possible objects from the
-- universe (Koan) and then applying f to the list of Bools.
evalQuant :: Maybe (Int -> JboProp)
          -> (Int -> JboProp)
          -> Koan
          -> ([Bool] -> Bool)
          -> BindfulTerm Sumti (OrError Bool)
evalQuant d p k f =
  fmap (fmap f . sequence . catMaybes) . sequence .
  fmap (flip withBoundVarBinding (\n ->
      do
      inDomain <- case d of
        Nothing -> return (Right True)
        Just d -> evalProp (d n) k
      case inDomain of
        Right True -> fmap Just $ evalProp (p n) k
        Right False -> return Nothing
        Left e -> return $ Just $ Left e
      )) $ sumtiInKoan k

evalProp :: JboProp -> Koan -> BindfulTerm Sumti (OrError Bool)
evalProp (Not p) k = fmap (fmap not) (evalProp p k)
evalProp (Connected c p q) k = do
  a <- evalProp p k
  b <- evalProp q k
  return $ do
    a <- a
    b <- b
    return $ (case c of
      And -> ( && )
      Or -> ( || )
      Impl -> (\x y -> (not x) || y)
      Equiv -> ( == )
      ) a b
evalProp (Quantified (LojQuantifier Exists) d p) k = evalQuant d p k (any id)
evalProp (Quantified (LojQuantifier Forall) d p) k = evalQuant d p k (all id)
evalProp (Quantified (LojQuantifier (Exactly n)) d p) k = evalQuant d p k ((== n) . boolCount)
evalProp (Quantified (MexQuantifier mex) d p) k =
  case evalMex mex of
    Right f -> evalQuant d p k (f . boolCount)
    Left e -> return $ Left e
evalProp (Rel r [Constant n a]) _ =
  case fixConstant r of
    Right bound -> do
      () <- bind (Constant n a) bound
      return $ Right True
    Left e -> return $ Left e
evalProp (Rel r ts) k = do
  ts <- mapTerms ts
  return $ do
    ts <- ts
    selbri <- selbriForRel r 
    return $ selbri k ts
evalProp (NonLogConnected _ _ _) _ = return $ Left "non-logical connectives not supported"
evalProp (Modal _ _) _ = return $ Left "modals not supported"
evalProp _ _ = return $ Left "some unknown element"

satisfiesRule :: Rule -> Koan -> OrError Bool
satisfiesRule ps k = evalBindful $ foldM (\state p ->
    case state of
      Right True -> evalProp p k
      other -> return other
  ) (Right True) ps
