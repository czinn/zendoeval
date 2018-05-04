module FixConstant where

import Data.Maybe (fromJust, fromMaybe)

import JboProp
import Logic

import Sumti
import Koan
import Selbri
import Error (OrError)

heightProperty :: Koan -> Sumti -> Int
heightProperty k s =
  case s of
    Sumti.Pyramid i j -> fromMaybe (-1) $ do
      part <- nth k i
      height <- nth (pyramidHeights part) j
      return height
    Ground -> 0
    _ -> -1

propertyForRel :: JboRel -> OrError Sumti
propertyForRel (Brivla "barda") = Right $ Property $ (\k s ->
    case getPyramid k s of
      Just (Koan.Pyramid z _) -> fromEnum z
      _ -> -1
  )
propertyForRel (Brivla "galtu") = Right $ Property heightProperty
propertyForRel (Brivla "dizlo") = Right $ Property $ \k s -> -(heightProperty k s)
propertyForRel r = Left $ "unsupported relation in property " ++ show r

fixConstant :: JboRel -> OrError Sumti
fixConstant (Brivla "loldi") = Right Ground
fixConstant (AbsPred _ (JboNPred 1 f)) =
  case f [Unfilled] of
    Rel r _ -> propertyForRel r
    p -> Left ("unsupported property " ++ show p)
fixConstant r = Left $ "unable to fix constant " ++ show r
