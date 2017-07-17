module Chap9 where


test x = x + 1

-- Exercises: Understanding Folds

-- 1.
fold1base = foldr (*) 1 [1..5]

-- a. error
--fold1A = flip (*) 1 [1..5]

-- b. same eval as fold1base
fold1B = foldl (flip (*)) 1 [1..5]

-- c. same eval as fold1base
fold1C = foldl (*) 1 [1..5]

-- 2.
evalFold = foldl (flip (*)) 1 [1..3]

evalFoldCheat = foldl f "1" (map show [1..3])
    where f = (\x y -> concat ["(", y, "*", x, ")"])
-- "(3*(2*(1*1)))"

-- 3.
-- c) I guess?

-- 4.
-- a.

-- 5.
-- origA = fixFoldA = foldr (++) ["woot", "WOOT", "woot"]
fixFoldA = foldr (++) "" ["woot", "WOOT", "woot"]

-- origB = foldr max [] "fear is the little death"
fixFoldB = foldr max (minBound::Char) "fear is the little death"

-- origC = foldr and True [False, True]
fixFoldC = foldr (&&) True [False, True]

-- what is the point of this? Always Returns True as True || x -> True, where x is True or False
origD = foldr (||) True [False, True]
--fixFoldD = foldr (||) True [False, True]

--origE = foldl ((++) . show) "" [1..5]
fixFoldE = foldr ((++) . show) "" [1..5]

--origF = foldr const 'a' [1..5]
fixFoldF = foldr (flip const) 'a' [1..5]

--origG = foldr const 0 "tacos"s
fixFoldG = foldr (flip const) 0 "tacos"

--origH = foldl (flip const) 0 "burritos"
fixFoldH = foldl const 0 "burritos"

--origI = foldl (flip const) 'z' [1..5]
fixFoldI = foldl const 'z' [1..5]