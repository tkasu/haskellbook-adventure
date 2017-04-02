{-# LANGUAGE NoMonomorphismRestriction #-}

module Chap5NoMono where

example = 1

-- Chapter Exercises continues
-- Determine the type

-- 1.

-- a. e1a :: Num => a
-- 54
e1a = (* 9) 6

-- b. e1b :: Num a => (a, [Char])
-- (0, "doge")
e1b = head [(0,"doge"),(1,"kitteh")]

-- c. e1c :: (Integer, [Char])
-- (0, "doge")
e1c = head [(0 :: Integer ,"doge"),(1,"kitteh")]


-- d. e1d :: Bool
-- False
e1d = if False then True else False

-- e. Int
-- 5
e1e = length [1, 2, 3, 4, 5]

-- f. e1f :: Bool
-- False
e1f = (length [1, 2, 3, 4]) > (length "TACOCAT")

-- 2.
-- e2w :: Num a => a 
e2x = 5
e2y = e2x + 5
e2w = e2y * 10

-- 3. e3z :: Num a => a -> a
e3z y = y * 10

