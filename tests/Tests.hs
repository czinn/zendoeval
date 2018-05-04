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

spotTests = "spot tests" ~: TestList $
  rulesWithKoans
    ["ci da barna de poi xunre"]
    [ ([Stack [p l r, p s b]], Right True)
    , ([Stack [p m r, p s b, p s r]], Right True)
    , ([Stack [p m r, p s b]], Right False)
    , ([Stack [p m r], Pointing Lft (p s r)], Right True)
    , ([Stack [p l r, p s r]], Right False)
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

concreteColourSizeTests = "concrete colour/size tests" ~: TestList $
  rulesWithKoans
    ["pa da nilbra ro de poi pirmidi"]
    [ ([Stack [p l r, p l b], Pointing Lft (p l g)], Right True)
    , ([Stack [p l r, p m b], Pointing Lft (p l g)], Right False)
    , ([Stack [p m r, p m b]], Right True)
    ]
  ++
  rulesWithKoans
    ["da se skari re de"]
    [ ([Stack [p l r, p s b], Stack [p s r]], Right True)
    , ([Stack [p l r, p s b], Stack [p s b]], Right True)
    , ([Stack [p l r, p s b], Stack [p s g]], Right False)
    , ([Stack [p l r, p s r], Stack [p s r]], Right False)
    ]

meksoTests = "mekso tests" ~: TestList $
  rulesWithKoans
    ["su'o re da pirmidi"]
    [ ([Stack [p l r]], Right False)
    , ([Stack [p l r, p m b]], Right True)
    , ([Stack [p l r, p m b, p s g]], Right True)
    ]
  ++
  rulesWithKoans
    ["su'e re da pirmidi"]
    [ ([Stack [p l r]], Right True)
    , ([Stack [p l r, p m b]], Right True)
    , ([Stack [p l r, p m b, p s g]], Right False)
    ]
  ++
  rulesWithKoans
    ["me'i re da pirmidi"]
    [ ([Stack [p l r]], Right True)
    , ([Stack [p l r, p m b]], Right False)
    , ([Stack [p l r, p m b, p s g]], Right False)
    ]
  ++
  rulesWithKoans
    ["za'u re da pirmidi"]
    [ ([Stack [p l r]], Right False)
    , ([Stack [p l r, p m b]], Right False)
    , ([Stack [p l r, p m b, p s g]], Right True)
    ]

groundTests = "ground tests" ~: TestList $
  rulesWithKoans
    ["da poi xunre cu pencu lo loldi"]
    [ ([Stack [p l r]], Right True)
    , ([Stack [p s b, p l r]], Right True)
    , ([Stack [p s b, p s r]], Right False)
    , ([Stack [p s b, p s r], Pointing Lft (p l r)], Right True)
    ]

comparisonTests = "comparison tests" ~: TestList $
  rulesWithKoans
    [ "da poi xunre cu zmadu ro de poi blanu ku'o lo ka barda"
    , "da poi xunre cu se mleca ro de poi blanu ku'o lo ka barda" ]
    [ ([Stack [p l r, p s b]], Right True)
    , ([Stack [p l r, p l b]], Right False)
    , ([Stack [p l r, p m b, p s b, p l g]], Right True)
    , ([Stack [p l r, p l b, p s b]], Right False)
    ]
  ++
  rulesWithKoans
    [ "da poi xunre cu zmadu ro de poi blanu ku'o lo ka galtu"
    , "da poi xunre cu mleca ro de poi blanu ku'o lo ka dizlo" ]
    [ ([Stack [p l r, p s b]], Right False)
    , ([Stack [p l b, p s r]], Right True)
    , ([Stack [p s r], Stack [p l b]], Right False)
    , ([Stack [p l g, p s r], Stack [p l b]], Right True)
    ]

-- Tests for every rule given on http://charleszinn.ca/blog/lojban-zendo/
-- The tests are not especially thorough in verifying that the semantics of every rule are correct;
-- they simply demonstrate a positive and negative example for each of the rules.
blogTests = "blog tests" ~: TestList $
  -- Logical Quantified Existential Variables
  ruleWithKoans "da xunre"
    [ ([Stack [p s r]], Right True), ([Stack [p s b]], Right False) ]
  :
  ruleWithKoans "pa da xunre"
    [ ([Stack [p s r]], Right True), ([Stack [p m r, p s r]], Right False) ]
  :
  ruleWithKoans "re da xunre"
    [ ([Stack [p m r, p s r]], Right True), ([Stack [p s r]], Right False) ]
  :
  ruleWithKoans "su'o re da xunre"
    [ ([Stack [p l r, p m r, p s r]], Right True), ([Stack [p s r, p s b]], Right False) ]
  :
  ruleWithKoans "no da xunre"
    [ ([Stack [p s b]], Right True), ([Stack [p s r]], Right False) ]
  :
  ruleWithKoans "ro da poi pirmidi ku'o xunre" -- originally "ro da xunre"
    [ ([Stack [p m r, p s r]], Right True), ([Stack [p m r, p s b]], Right False) ]
  :
  ruleWithKoans "da farsni de"
    [ ([Stack [p s r, p s r]], Right True), ([Stack [p s r], Stack [p s r]], Right False) ]
  :
  ruleWithKoans "no da farsni su'o re de"
    [ ([Stack [p m r, p s r]], Right True), ([Stack [p l r, p m r, p s r]], Right False) ]
  :
  ruleWithKoans "da farsni re de"
    [ ([Stack [p l r, p m r, p s r]], Right True), ([Stack [p m r, p s r]], Right False) ]
  :
  rulesWithKoans
    [ "da poi pirmidi ku'o farsni no de" -- originally "da farsni no de"
    , "da poi pirmidi ku'o no de zo'u da farsni de" -- originally "da no de zo'u da farsni de"
    ]
    [ ([Stack [p s r], Pointing Lft (p s r)], Right True)
    , ([Pointing Rgt (p s r), Pointing Lft (p s r)], Right False) ]
  ++
  rulesWithKoans
    [ "no de se farsni da"
    , "no de da zo'u da farsni de" ]
    [ ([Stack [p s r], Pointing Rgt (p s r)], Right True)
    , ([Stack [p s r], Pointing Lft (p s r)], Right False) ]
  ++
  -- Restrictive Relative Clauses
  ruleWithKoans "ro da poi blanu ku'o sraji"
    [ ([Stack [p m b, p s b]], Right True), ([Stack [p m b], Pointing Lft (p s b)], Right False) ]
  :
  ruleWithKoans "ro da poi farsni de ku'o xunre"
    [ ([Stack [p s b], Pointing Lft (p s r)], Right True)
    , ([Stack [p s r], Pointing Lft (p s b)], Right False) ]
  :
  ruleWithKoans "da poi pirmidi ku'o pencu de poi farsni pa di" -- originally "da pencu de poi farsni pa di"
    [ ([Stack [p s r], Pointing Rgt (p s r), Empty, Stack [p s r]], Right True)
    , ([Stack [p s r], Empty, Pointing Rgt (p s r), Empty, Stack [p s r]], Right False)
    , ([Stack [p s r], Pointing Rgt (p s r), Empty, Stack [p s r], Stack [p s r]], Right False) ]
  :
  -- Logical Connectives
  ruleWithKoans "da sraji gi'e blanu"
    [ ([Stack [p s b]], Right True), ([Stack [p s g], Pointing Lft (p s b)], Right False) ]
  :
  ruleWithKoans "da sraji gi'a blanu"
    [ ([Stack [p s g], Pointing Lft (p s b)], Right True), ([Pointing Lft (p s g)], Right False) ]
  :
  ruleWithKoans "da poi pirmidi ku'o sraji na gi'e nai blanu" -- originally "da sraji na gi'e nai blanu"
    [ ([Stack [p s g], Pointing Lft (p s b)], Right False), ([Pointing Lft (p s g)], Right True) ]
  :
  ruleWithKoans "da poi pirmidi ku'o sraji gi'o blanu" -- originally "da sraji gi'o blanu"
    [ ([Stack [p s b]], Right True), ([Pointing Lft (p s g)], Right True), ([Stack [p s g]], Right False) ]
  :
  ruleWithKoans "ro da poi pirmidi ku'o sraji gi'o blanu" -- originally "ro da sraji gi'o blanu"
    [ ([Stack [p s b]], Right True), ([Pointing Lft (p s g)], Right True), ([Stack [p s g]], Right False) ]
  :
  rulesWithKoans ["ro da barda na gi'a xunre", "ro da poi barda ku'o xunre"]
    [ ([Stack [p l r, p s g]], Right True), ([Stack [p l g, p s r]], Right False) ]
  ++
  ruleWithKoans "da sraji .i jo no de blanu"
    [ ([Stack [p l r]], Right True), ([Pointing Lft (p s b)], Right True), ([Stack [p s b]], Right False) ]
  :
  ruleWithKoans "ro da poi pirmidi ku'o blanu gi'a xunre gi'e barda" -- originally "ro da blanu gi'a xunre gi'e barda"
    [ ([Stack [p l r, p l b]], Right True), ([Stack [p l g, p l b]], Right False) ]
  :
  ruleWithKoans "ro da poi pirmidi ku'o blanu gi'e barda gi'a xunre" -- originally "ro da blanu gi'e barda gi'a xunre"
    [ ([Stack [p l b, p s r]], Right True), ([Stack [p l r, p s b]], Right False) ]
  :
  -- Comparisons
  ruleWithKoans "da poi pirmidi ku'o zmadu de poi pirmidi ku'o lo ka barda" -- originally "da zmadu de lo ka barda"
    [ ([Stack [p m r, p s r]], Right True), ([Stack [p m r, p m r]], Right False) ]
  :
  ruleWithKoans "da poi xunre ku'o zmadu de poi blanu ku'o lo ka barda"
    [ ([Stack [p l b, p m r, p s b]], Right True), ([Stack [p m r, p m b]], Right False) ]
  :
  ruleWithKoans "da poi xunre ku'o zmadu ro de poi blanu ku'o lo ka barda"
    [ ([Stack [p m r, p s b]], Right True), ([Stack [p l b, p m r, p s b]], Right False) ]
  :
  ruleWithKoans "da poi traji lo ka galtu ku'o xunre"
    [ ([Stack [p s b, p s r]], Right True), ([Stack [p s b, p s r], Stack [p l g]], Right False) ]
  :
  -- Sample Rules
  ruleWithKoans "su'o re da pelxu"
    [ ([Stack [p s y, p m y]], Right True), ([Stack [p s b, p m b]], Right False) ]
  :
  ruleWithKoans "re da blanu gi'a cmalu"
    [ ([Stack [p l g, p m b, p s r]], Right True), ([Stack [p l b, p m b, p s r]], Right False) ]
  :
  ruleWithKoans "no da norbra gi'e crino"
    [ ([Stack [p l g, p m b]], Right True), ([Stack [p l b, p m g]], Right False) ]
  :
  ruleWithKoans "da se skari su'o re de"
    [ ([Stack [p l g, p m b, p s g]], Right True), ([Stack [p l g, p m b, p s r]], Right False) ]
  :
  ruleWithKoans "ci da barna de poi xunre"
    [ ([Stack [p m r, p s b, p s r]], Right True) , ([Stack [p m r, p s b]], Right False) ]
  :
  ruleWithKoans "pa da nilbra ro de poi sraji"
    [ ([Stack [p m r, p m b], Pointing Lft (p s g)], Right True)
    , ([Stack [p m r, p s g], Pointing Lft (p m b)], Right False) ]
  :
  ruleWithKoans "da poi blanu ku'o pencu lo loldi"
    [ ([Stack [p l b, p s r]], Right True), ([Stack [p l r, p s b]], Right False) ]
  :
  ruleWithKoans "da farsni de poi mleca da lo ka barda"
    [ ([Stack [p l b, p s r]], Right True), ([Stack [p s r, p l b]], Right False) ]
  :
  ruleWithKoans "ro da poi pinta ku'o farsni de poi sraji"
    [ ([Stack [p m r], Pointing Lft (p s g)], Right True), ([Stack [p m r], Pointing Rgt (p s g)], Right False) ]
  : []

allTests = TestList $
  [ basicTests
  , spotTests
  , touchingTests
  , pointingTests
  , columnTests
  , concreteColourSizeTests
  , meksoTests
  , groundTests
  , comparisonTests
  , blogTests
  ]

main = runTestTT allTests
