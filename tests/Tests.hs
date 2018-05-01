import Test.HUnit

import ZendoParse (tersmu)
import ZendoEval (satisfiesRule)
import Koan

satisfiesString :: String -> Koan -> Maybe Bool
satisfiesString s k = do
  rule <- tersmu s
  result <- satisfiesRule rule k
  return $ result

koanTest :: String -> Koan -> Maybe Bool -> Test
koanTest s k e = show k ~: satisfiesString s k ~?= e

ruleWithKoans :: String -> [(Koan, Maybe Bool)] -> Test
ruleWithKoans s ks = s ~: TestList $ fmap (\(k, e) -> koanTest s k e) ks

rulesWithKoans :: [String] -> [(Koan, Maybe Bool)] -> [Test]
rulesWithKoans ss ks = fmap (flip ruleWithKoans $ ks) ss

p = Pyramid
s = Small
m = Medium
l = Large
b = Blue
g = Green
r = Red
y = Yellow

basicTests = "basic tests" ~: TestList $
  rulesWithKoans
    ["da xunre", "su'o da xunre"]
    [ ([Stack [p l r, p s b]], Just True)
    , ([Stack [p s y, p s r]], Just True)
    , ([Stack [p l y, p m b]], Just False)
    , ([Pointing Koan.Left (p m r)], Just True)
    ]
  ++
  rulesWithKoans
    ["da blanu gi'e cmalu", "da ge blanu gi cmalu", "da zo'u da cmalu .i je da blanu"]
    [ ([Stack [p l r, p s b]], Just True)
    , ([Pointing Koan.Right (p s b)], Just True)
    , ([Stack [p l b, p s y]], Just False)
    ]
  ++
  rulesWithKoans
    ["da na blanu", "na ku ro da blanu"]
    [ ([Stack [p l b, p s y]], Just True)
    , ([Stack [p l b]], Just False)
    , ([Stack [p s b], Stack [p m b]], Just False)
    ]
  ++
  rulesWithKoans
    ["na ku da xunre", "ro da poi pirmidi na xunre"]
    [ ([Stack [p m r], Stack [p l g]], Just False)
    , ([Stack [p l g, p m y, p s b]], Just True)
    , ([], Just True)
    ]

quantifierTests = "quantifier tests" ~: TestList $
  rulesWithKoans
    ["pa da xunre"]
    [ ([Stack [p l r, p s b]], Just True)
    , ([Stack [p l r, p s r]], Just False)
    , ([Stack [p l r], Pointing Koan.Right (p s r)], Just False)
    , ([Stack [p l g], Pointing Koan.Right (p s y)], Just False)
    , ([Stack [p m r]], Just True)
    ]

touchingTests = "touching tests" ~: TestList $
  rulesWithKoans
    ["da poi xunre cu pencu de poi blanu"]
    -- Single stack
    [ ([Stack [p l r, p s b]], Just True)
    , ([Stack [p s b, p l r]], Just False)
    , ([Stack [p m b, p l r]], Just True)
    , ([Stack [p s b, p m r]], Just True)
    , ([Stack [p s b, p s g, p m r]], Just False)
    , ([Stack [p l b, p s g, p m r]], Just False)
    -- Two stacks
    , ([Stack [p s b], Stack [p l r]], Just True)
    , ([Stack [p m b], Stack [p l r]], Just True)
    , ([Stack [p m b, p m g], Stack [p m r, p m g]], Just True)
    , ([Stack [p m g, p m b], Stack [p m r, p m g]], Just False)
    , ([Stack [p m g, p m b], Stack [p m g, p m r]], Just True)
    , ([Stack [p m g, p m b], Stack [p m g, p s r]], Just False)
    , ([Stack [p m g, p m b], Stack [p m g, p l r]], Just False)
    , ([Stack [p m g, p s g, p l b], Stack [p m g, p m r]], Just True)
    , ([Stack [p m g, p s g, p l b], Stack [p m g, p l r]], Just True)
    , ([Stack [p m b, p s g, p l g], Stack [p m g, p l r]], Just True)
    , ([Stack [p s b, p s g, p s g], Stack [p l r]], Just True)
    , ([Stack [p s b, p s g, p s g, p l g ], Stack [p l r]], Just False)
    , ([Stack [p s b], Empty, Stack [p s r]], Just False)
    -- Stack and pointing
    , ([Stack [p s b], Pointing Koan.Left (p s r)], Just True)
    , ([Stack [p s b], Pointing Koan.Right (p s r)], Just True)
    , ([Stack [p s b, p m g], Pointing Koan.Left (p s r)], Just False)
    , ([Stack [p s b, p s g], Pointing Koan.Left (p s r)], Just True)
    , ([Stack [p s b, p s g, p s g, p l g], Pointing Koan.Left (p s r)], Just True)
    , ([Stack [p s b, p s g, p s g, p l g], Pointing Koan.Right (p s r)], Just False)
    , ([Stack [p s g, p s g, p s g, p l b], Pointing Koan.Right (p s r)], Just True)
    , ([Stack [p s b, p s g, p s g, p s g, p l g], Pointing Koan.Right (p s r)], Just True)
    , ([Stack [p s b], Empty, Pointing Koan.Left (p s r)], Just False)
    -- Two pointing
    , ([Pointing Koan.Left (p s b), Pointing Koan.Left (p s r)], Just True)
    , ([Pointing Koan.Right (p s b), Pointing Koan.Left (p s r)], Just True)
    , ([Pointing Koan.Left (p s b), Pointing Koan.Right (p s r)], Just True)
    , ([Pointing Koan.Right (p s b), Pointing Koan.Right (p s r)], Just True)
    , ([Pointing Koan.Right (p s b), Empty, Pointing Koan.Left (p s r)], Just False)
    ]

pointingTests = "pointing tests" ~: TestList $
  rulesWithKoans
    ["da poi xunre cu farsni de poi blanu"]
    -- Single stack
    [ ([Stack [p s r, p s b]], Just True)
    , ([Stack [p s b, p s r]], Just False)
    , ([Stack [p s r, p s g, p s b]], Just True)
    -- Pointing at a stack
    , ([Pointing Koan.Right (p s r), Stack [p s g, p s b]], Just False)
    , ([Pointing Koan.Right (p s r), Stack [p m g, p l b]], Just True)
    , ([Pointing Koan.Right (p s r), Stack [p s b]], Just True)
    , ([Stack [p s b], Pointing Koan.Left (p s r)], Just True)
    , ([Pointing Koan.Left (p s r), Stack [p s b]], Just False)
    , ([Stack [p s b], Pointing Koan.Right (p s r)], Just False)
    -- Pointing at another pointing pyramid
    , ([Pointing Koan.Right (p s r), Pointing Koan.Right (p s b)], Just True)
    , ([Pointing Koan.Right (p s b), Pointing Koan.Left (p s r)], Just True)
    , ([Pointing Koan.Left (p s r), Pointing Koan.Right (p s b)], Just False)
    , ([Pointing Koan.Right (p s b), Pointing Koan.Right (p s r)], Just False)
    ]

allTests = TestList $ [basicTests, quantifierTests, touchingTests, pointingTests]

main = runTestTT allTests
