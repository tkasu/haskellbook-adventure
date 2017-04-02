module Chap5 where

import Lib

-- CHAPTER 5

-- Exercises: Type Matching

-- 1.
-- not :: Bool -> Bool

-- length :: [a] -> Integer
-- real length :: Foldable t => t a -> Int

-- concat :: Foldable t => t [a] -> [a]

-- head :: Foldable t => t a => a
-- real head :: [a] -> a

-- (<) :: Ord a => a -> a -> Bool

-- f t
-- a c
-- b d
-- c b
-- d a
-- e e


-- Exercises: Type Arguments

-- 1. a.
f1t :: a -> a -> a -> a
f1t = undefined

x1t :: Char
x1t = undefined

r1t = f1t x1t 

-- 2. d.
g2t :: a -> b -> c -> b
g2t = undefined

r2t = g2t 0 'c' "woot"

-- 3. b.

h3t :: (Num a, Num b) => a -> b -> b
h3t = undefined

r3t = h3t 1.0 2

-- 4. c.

h4t :: (Num a, Num b) => a -> b -> b
h4t = undefined

r4t = h4t 1 (5.5 :: Double)

-- 5. a.
jackal :: (Ord a, Eq b) => a -> b -> a
jackal = undefined

pumpedJackal = jackal "keyboard" "has the word jackal in it"

-- 6. c.
partitionJackal = "keyboard"

-- 7. a.
kessel :: (Ord a, Num b) => a -> b -> a
kessel = undefined

pumpedKessel = kessel 1 2

-- 8. f (Thought it was e)
pumpedKessel2 = kessel 1 (2 :: Integer)

-- 9. c.
pumpedKessel3 = kessel (1 :: Integer) 2 

-- Exercies: Apply Yourself

-- 1. (++) :: [Char] -> [Char] -> [Char], when
myConcat x = x ++ "yo"

-- 2. (*) Fractional => a -> a -> a, when
myMult x = (x / 3) * 5

-- 3. take :: Int -> [Char] -> [Char], when 
myTake x = take x "hey you"

-- 4. (>) :: Int -> Int -> Bool, when 
myCom x = x > (length [1..10])

-- 5. (<) :: Char -> Char -> Bool, when
myAlph x = x < '<'


-- Chapter Exercies:

-- 1. c.
-- 2. a.
-- 3. b. ??
-- 4. c. ??


