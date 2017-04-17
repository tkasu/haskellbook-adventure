{-# OPTIONS -Wall #-}

module Chap7 where

-- import Lib

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


-- Exercises: Case Practice

-- 1.

functionC x y = if (x > y) then x else y

functionC' x y =
  case x > y of
    True -> x
    False -> y

-- 2.

ifEvenAdd2 n = if even n then (n + 2) else n

ifEvenAdd2' n =
  case even n of
    True -> n + 2
    False -> n

-- 3.

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1

nums' x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Exercises: Artful Dodgy

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

d2 = dodgy 1 1
d3 = dodgy 2 2
d4 = dodgy 1 2
d5 = dodgy 2 1
d6 = oneIsOne 1
d7 = oneIsOne 2
d8 = oneIsTwo 1
d9 = oneIsTwo 2
d10 = oneIsOne 3
d11 = oneIsTwo 3

-- Exercise: Guard Duty

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100

-- 1.
-- test for top-most otherwise (makes no sense)
-- all inputs yield 'F'
avgGradeBroken :: (Fractional a, Ord a) => a -> Char
avgGradeBroken x
  | otherwise = 'F'
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  where y = x / 100


-- 2.
-- Gruard reorder test
-- Inputs >= 70 yield result 'C'
avgGradeBroken2 :: (Fractional a, Ord a) => a -> Char
avgGradeBroken2 x
  | y >= 0.7  = 'C'
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100

-- 3., 4., 5.
-- b. 
pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise        = False

-- 6., 7., 8.
-- c.
numbers ::(Ord a, Num a, Num b) => a -> b
numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1

-- Chapter Exercises

-- Multiple choise

-- 1. d.
-- 2. b.
-- 3. d.
-- 4. b.

-- 5.
-- :t fChap55 True
-- fChapE True :: Bool
fChapE5 :: a -> a
fChapE5 x = x

-- Let's write code

-- 1.

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

-- a), b)
-- This is just weird, so maybe I am missing something
tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast = fst . divMod x $ 10
        d     = snd . divMod xLast $ 10

--c)
hunsDigit :: Integral a => a -> a
hunsDigit x = d
  where xLast = fst . divMod x $ 100
        d     = snd . divMod xLast $ 10

-- extra
nthDigit :: Integral a => a -> a -> a
nthDigit x n = digit
  where divisor = 10 ^ (n - 1)
        xLast   = fst . divMod x $ divisor
        digit   = snd . divMod xLast $ 10

-- 2.

foolBool3 :: a -> a -> Bool -> a
foolBool3 x y True  = x
foolBool3 x y False = y

foolBool1 :: a -> a -> Bool -> a
foolBool1 x y bool =
  case bool of
    True -> x
    False -> y

foolBool2 :: a -> a -> Bool -> a
foolBool2 x y bool
  | bool == True = x
  | bool == False = y

-- 3.

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

