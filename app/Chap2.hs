module Chap2 where

import Lib
import Data.List

-- CHAPTER 2

-- Exercises: Heal the sick

-- 1.
area :: Double -> Double
area x = 3.14 * (x * x)

-- 2.
double :: Double -> Double
double x = x * 2

-- 3.
x = 7
y = 10
f = x + y

-- Exercies: A Head Code

-- introduction
mult1 :: Integer
mult1     = x * y
  where x = 5
        y = 6

-- 1.
myFunc1 :: Integer
myFunc1   = x * 3 + y
  where x = 3
        y = 1000

-- 2.
myFunc2 :: Integer
myFunc2   = x * 5
  where y = 10
        x = 10 * 5 + y

-- 3.
myFunc3 :: Double
myFunc3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

-- Chapter 2 exercises more with functions

z2 = 7
x2 = y2 ^ 2
waxOn = x2 * 5
y2 = z2 + 8

-- with where
waxOn2 :: Integer
waxOn2    = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

-- triple
triple :: Num a => a -> a
triple x = x * 3

-- waxOff
waxOff :: Num a => a -> a
waxOff x = triple x
