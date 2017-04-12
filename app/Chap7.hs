module Chap7 where

import Lib

--test x = x * 42

-- Exercises: Grap Bag

-- 1. 

mTh1 x y z = x * y * z
test1 = mTh1 99 2 27

mTh2 x y = \z -> x * y * z
test2 = mTh2 99 2 27
  
mTh3 x = \y -> \z -> x * y * z
test3 = mTh3 99 2 27

mTh4 = \x -> \y -> \z -> x * y * z
test4 = mTh4 99 2 27

testBool1 :: Bool
testBool1 = test1 == test2 && test1 == test3 && test1 == test4
-- True! 

-- 2.
-- b)

-- 3.

-- a)

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f n = n + 1

addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- b)

addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5

-- c)

mflip f = \x -> \y -> f y x

mflip' f x y = f y x


-- Exercises: Variety Pack

-- 1.

-- a), b)

k :: (x, y) -> x
k (x, y) = x

k1 :: Integer
k1 = k ((4 - 1), 10)

k2 :: String
k2 = k ("three", (1 + 2))

k3 :: Integer
k3 = k (3, True)

-- c)
-- k1, k3

-- 2.

fV2 :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
fV2 (a, b, c) (d, e, f) = ((a, d), (c, f))

