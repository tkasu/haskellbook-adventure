module Chap8 where

import Data.List (intersperse)
import Data.Char (digitToInt, intToDigit)

-- Chapter Exercises

-- Review of types

-- 1. c)
-- 2.

-- 3.
func :: [a] -> [a] -> [a]
func x y = x ++ y

-- d)

-- 4.
-- b)

-- Review currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. "woops mrow woohoo!"
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. "woops mrow blue mrow haha"
-- 5. "pink mrow haha mrow green mrow woops mrow blue"
-- 6. "are mrow Pugs mrow awesome"

-- Recursion

-- 1.
-- not in the mood right now

-- 2.

sumUpTo :: (Eq a, Num a) => a -> a
sumUpTo n
  | n == 0 = 0
  | otherwise = n + (sumUpTo (n - 1))

sumUpToAlt :: (Eq a, Num a) => a -> a
sumUpToAlt n = go 0 n
  where go i n
          | i == n = i
          | otherwise = i + (go (i + 1) n)

-- 3.

recurMult :: (Integral a) => a -> a -> a
recurMult x y = go a t 0 0
  where
    a = if y < 0 then -x else x
    t = abs y      
    go add target acc count
          | count == target = acc
          | otherwise = go add target (acc + a) (count + 1) 

-- Fixing dividedBy
-- original
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denum = go num denum 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)


-- The behavior of this is not the same as it's with divMod
-- e.g. divMod 11 (-2) = (-6, -1), dividedByAlt 11 (-2) = (Result (-5), Result 1)
-- e.g. divMod 10 0 = *** Exception: divided by zerro,
-- dividedByAlt 10 0 = (DividedByZero, DividedByZero)
-- I also believe that this is also far away from state of art function,
-- but the learning continues
data DividedResult =
  Result Integer
  | DividedByZero
  deriving Show 

dividedByAlt :: Integral a => a -> a -> (DividedResult, DividedResult)
dividedByAlt num denum = go num denum 0 incVal
  where
    incVal
      | num < 0 && denum >= 0 = -1
      | denum < 0 && num >= 0 = -1
      | otherwise = 1
    go :: Integral a => a -> a -> Integer -> Integer -> (DividedResult, DividedResult)
    go n d count i
      | d == 0 = (DividedByZero, DividedByZero)
      | (abs n) < (abs d) = (Result count, Result (toInteger n))
      | otherwise = go ((abs n) - (abs d)) d (count + i) i
      
-- McCarthy 91

mc91 :: Integer -> Integer
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 . mc91 $ (n + 11)


-- Numbers into words

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "error"

digits :: Int -> [Int]
digits n = go nCharList
  where nCharList  = show n
        go []      = []
        go (x:xs)  = (digitToInt x) : (go xs)

wordNumber :: Int -> String
wordNumber n = concat . (intersperse "-") $ digitsAsWords
  where digitsAsWords = map digitToWord (digits n)
        

