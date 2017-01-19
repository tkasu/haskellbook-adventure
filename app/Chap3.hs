module Chap3 where

import Lib

-- CHAPTER 3

-- Exercises: Scope

-- 3.
area :: Double -> Double
area d    = pi * (r * r)
  where r = d / 2

-- Exercies: Syntax Errors

-- 1.
-- ++ [1, 2, 3] [4, 5, 6]
listAppend :: [a] -> [a] -> [a]
listAppend a b = (++) a b 

-- 2.
-- '<3' ++ ' Haskell'
iHeartHaskell :: String
iHeartHaskell = "<3" ++ " Haskell"

-- 3.
-- no syntax error

-- Chapter Exercises

-- 1.
  -- a. ok
  -- b. error, see listAppend
  -- c. ok
  -- d. ok
  -- e. reverse "hello" !! 4
  -- f. ok
  -- g. error, take 4 "lovely"
  -- h. ok

-- 2.
  -- a. d.
  -- b. c.
  -- c. e.
  -- d. a.
  -- e. b.

-- Building functions

-- 2.

-- a.
dropLast :: [a] -> [a]
dropLast x  = take (len - 1) x
  where len = length x

-- b.
listWithFifth :: [a] -> [a]
listWithFifth x   = fifth : []
  where fifth = x !! 4

-- 3.
thirdLetter :: String -> Char
thirdLetter x = x !! 2

-- 4.
letterIndex :: Int -> Char
letterIndex x = str !! (x - 1)
  where   str = "Curry is awesome"

-- 5.
rvrs :: [a] -> [a]
rvrs []      = []
rvrs x       = last : rvrs (dropLast x)
  where len  = length x
        last = x !! (len - 1)

-- See Reverse.hs for the final new Module
