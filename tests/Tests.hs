import Test.HUnit

import ZendoParse (tersmu)
import ZendoEval (satisfiesRule)
import Error (OrError)
import Koan

satisfiesString :: String -> Koan -> OrError Bool
satisfiesString s k = do
  rule <- tersmu s
  result <- satisfiesRule rule k
  return $ result

koanTest :: String -> Koan -> OrError Bool -> Test
koanTest s k e = show k ~: satisfiesString s k ~?= e

ruleWithKoans :: String -> [(Koan, OrError Bool)] -> Test
ruleWithKoans s ks = s ~: TestList $ fmap (\(k, e) -> koanTest s k e) ks

rulesWithKoans :: [String] -> [(Koan, OrError Bool)] -> [Test]
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
    [ ([Stack [p l r, p s b]], Right True)
    , ([Stack [p s y, p s r]], Right True)
    , ([Stack [p l y, p m b]], Right False)
    , ([Pointing Lft (p m r)], Right True)
    ]
  ++
  rulesWithKoans
    ["da blanu gi'e cmalu", "da ge blanu gi cmalu", "da zo'u da cmalu .i je da blanu"]
    [ ([Stack [p l r, p s b]], Right True)
    , ([Pointing Rgt (p s b)], Right True)
    , ([Stack [p l b, p s y]], Right False)
    ]
  ++
  rulesWithKoans
    ["da poi pirmidi cu na blanu", "na ku ro da poi pirmidi cu blanu"]
    [ ([Stack [p l b, p s y]], Right True)
    , ([Stack [p l b]], Right False)
    , ([Stack [p s b], Stack [p m b]], Right False)
    ]
  ++
  rulesWithKoans
    ["na ku da xunre", "ro da poi pirmidi na xunre"]
    [ ([Stack [p m r], Stack [p l g]], Right False)
    , ([Stack [p l g, p m y, p s b]], Right True)
    , ([], Right True)
    ]
  ++
  rulesWithKoans
    ["pa da xunre"]
    [ ([Stack [p l r, p s b]], Right True)
    , ([Stack [p l r, p s r]], Right False)
    , ([Stack [p l r], Pointing Rgt (p s r)], Right False)
    , ([Stack [p l g], Pointing Rgt (p s y)], Right False)
    , ([Stack [p m r]], Right True)
    ]

touchingTests = "touching tests" ~: TestList $
  rulesWithKoans
    ["da poi xunre cu pencu de poi blanu"]
    -- Single stack
    [ ([Stack [p l r, p s b]], Right True)
    , ([Stack [p s b, p l r]], Right False)
    , ([Stack [p m b, p l r]], Right True)
    , ([Stack [p s b, p m r]], Right True)
    , ([Stack [p s b, p s g, p m r]], Right False)
    , ([Stack [p l b, p s g, p m r]], Right False)
    -- Two stacks
    , ([Stack [p s b], Stack [p l r]], Right True)
    , ([Stack [p m b], Stack [p l r]], Right True)
    , ([Stack [p m b, p m g], Stack [p m r, p m g]], Right True)
    , ([Stack [p m g, p m b], Stack [p m r, p m g]], Right False)
    , ([Stack [p m g, p m b], Stack [p m g, p m r]], Right True)
    , ([Stack [p m g, p m b], Stack [p m g, p s r]], Right False)
    , ([Stack [p m g, p m b], Stack [p m g, p l r]], Right False)
    , ([Stack [p m g, p s g, p l b], Stack [p m g, p m r]], Right True)
    , ([Stack [p m g, p s g, p l b], Stack [p m g, p l r]], Right True)
    , ([Stack [p m b, p s g, p l g], Stack [p m g, p l r]], Right True)
    , ([Stack [p s b, p s g, p s g], Stack [p l r]], Right True)
    , ([Stack [p s b, p s g, p s g, p l g ], Stack [p l r]], Right False)
    , ([Stack [p s b], Empty, Stack [p s r]], Right False)
    -- Stack and pointing
    , ([Stack [p s b], Pointing Lft (p s r)], Right True)
    , ([Stack [p s b], Pointing Rgt (p s r)], Right True)
    , ([Stack [p s b, p m g], Pointing Lft (p s r)], Right False)
    , ([Stack [p s b, p s g], Pointing Lft (p s r)], Right True)
    , ([Stack [p s b, p s g, p s g, p l g], Pointing Lft (p s r)], Right True)
    , ([Stack [p s b, p s g, p s g, p l g], Pointing Rgt (p s r)], Right False)
    , ([Stack [p s g, p s g, p s g, p l b], Pointing Rgt (p s r)], Right True)
    , ([Stack [p s b, p s g, p s g, p s g, p l g], Pointing Rgt (p s r)], Right True)
    , ([Stack [p s b], Empty, Pointing Lft (p s r)], Right False)
    -- Two pointing
    , ([Pointing Lft (p s b), Pointing Lft (p s r)], Right True)
    , ([Pointing Rgt (p s b), Pointing Lft (p s r)], Right True)
    , ([Pointing Lft (p s b), Pointing Rgt (p s r)], Right True)
    , ([Pointing Rgt (p s b), Pointing Rgt (p s r)], Right True)
    , ([Pointing Rgt (p s b), Empty, Pointing Lft (p s r)], Right False)
    ]

pointingTests = "pointing tests" ~: TestList $
  rulesWithKoans
    ["da poi xunre cu farsni de poi blanu"]
    -- Single stack
    [ ([Stack [p s r, p s b]], Right True)
    , ([Stack [p s b, p s r]], Right False)
    , ([Stack [p s r, p s g, p s b]], Right True)
    -- Pointing at a stack
    , ([Pointing Rgt (p s r), Stack [p s g, p s b]], Right False)
    , ([Pointing Rgt (p s r), Stack [p m g, p l b]], Right True)
    , ([Pointing Rgt (p s r), Stack [p s b]], Right True)
    , ([Stack [p s b], Pointing Lft (p s r)], Right True)
    , ([Pointing Lft (p s r), Stack [p s b]], Right False)
    , ([Stack [p s b], Pointing Rgt (p s r)], Right False)
    -- Pointing at another pointing pyramid
    , ([Pointing Rgt (p s r), Pointing Rgt (p s b)], Right True)
    , ([Pointing Rgt (p s b), Pointing Lft (p s r)], Right True)
    , ([Pointing Lft (p s r), Pointing Rgt (p s b)], Right False)
    , ([Pointing Rgt (p s b), Pointing Rgt (p s r)], Right False)
    ]

columnTests = "column tests" ~: TestList $
  rulesWithKoans
    ["da se pagbu re de"]
    [ ([Stack [p l r, p s b]], Right True)
    , ([Stack [p l r], Stack [p s b]], Right False)
    , ([Stack [p l r, p m r, p s r], Stack [p s b]], Right False)
    , ([Stack [p l r, p m r], Stack [p s b]], Right True)
    ]
  ++
  rulesWithKoans
    ["da pagbu de"]
    [ ([Stack [p l r]], Right True)
    , ([Stack [p l r], Pointing Lft (p s r)], Right True)
    , ([Pointing Rgt (p l r), Pointing Lft (p s r)], Right False)
    ]
  ++
  rulesWithKoans
    ["re da pagbu de", "re da sraji"]
    [ ([Stack [p l r, p s b]], Right True)
    , ([Stack [p l r], Stack [p s b]], Right True)
    , ([Stack [p l r, p m r], Stack [p s b]], Right False)
    , ([Stack [p l r], Pointing Lft (p s g), Stack [p s b]], Right True)
    ]

allTests = TestList $ [basicTests, touchingTests, pointingTests, columnTests]

main = runTestTT allTests
