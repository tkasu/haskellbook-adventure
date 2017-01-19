module Chap4 where

import Lib
import Data.Tuple

-- CHAPTER 4


-- Exercise: Mood Swing
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

-- Exericise: Find the Mistakes

mistake1 = not True && True

mistake2 x = not (x == 6)

-- no errors
mistake3 = (1 * 2) > 5

-- Which one is happier, Merry or Happy?
data Happiness = Merry | Happy deriving (Show, Eq, Ord)

mistake4 = [Merry] > [Happy]

-- [1, 2, 3] ++ "look at me!"
-- no possible way to combine Integer and String, so lets convert all to Strings

mistakeList5 :: [Integer]
mistakeList5 = [1, 2, 3]

itemsToString :: Show a => [a] -> [String]
itemsToString [] = []
itemsToString x = map show x

combIntListToStr x y = xStr ++ y : []
  where xStr = itemsToString x

mistake5 = combIntListToStr mistakeList5 "look at me!"


-- Chapter exercises

-- pre given data:

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

-- 1. 
-- :t length
-- length :: Foldable t => t a => Int

-- 2.

lengthA :: Int
lengthA = length [1, 2, 3, 4, 5]

lengthB :: Int
lengthB = length [(1, 2), (2, 3), (3, 4)]

lengthC :: Int
lengthC = length allAwesome

lengthD :: Int
lengthD = length (concat allAwesome)

-- 3.

myDiv1 = 6 / 9

-- length returns Int, Int is not fractional

-- 4.

-- version with Integer division 
myDivInt2 = div 6 (length [1, 2, 3])

-- version that supports fractionals and returns Double
myDivFrac2 = 6 / (realToFrac (length [1, 2, 3]))

-- 5.

myBool = 2 + 3 == 5
-- :t myBool
-- myBool : Bool

-- 6.

myBool2   = x + 3 == 5
  where x = 5

-- 7.

exp1 = length allAwesome == 2
-- 2 == 2
-- True

-- exp2 = length [1, 'a', 3, 'b']
-- error, Varying types in List

exp3 = length allAwesome + length awesome
-- 2 + 3
-- 5

exp4 = (8 == 8) && ('b' < 'a')
-- True && False
-- False


-- exp5 = (8 == 8) && 9
-- 9 does not reduce to True | False

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

-- 9.
myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-1) * x else x

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = (,) ((snd x), (snd y)) ((fst x), (fst y))

-- Correcting syntax

-- 1.
myPlus = (+)

lenPlusOne xs = w `myPlus` 1
  where w = length xs

-- 2.
myId :: a -> a
myId a = a

-- 3. 
listHead :: [a] -> a
listHead (x : xs) = x 

-- 4.
tupleHead :: (a, b) -> a
tupleHead (x, y) = x

-- Match the function names to their types

-- 1.

-- a.
-- show a => a -> String
-- show is a function, not a typeclass
--myTest1 :: show a => a -> String
--myTest1 x = show x

-- b. 
-- Show a -> a -> String
-- Show a is not a type but typeclass
-- myTest2 :: Show a -> a -> String
-- myTest2 x = show x

-- c.
-- Show a => a -> String
myTest3 :: Show a => a -> String
myTest3 x = show x

-- 2.

-- a.
-- a -> a -> Bool
-- error x, y need to implement Eq
-- myTest4 :: a -> a -> Bool
-- myTest4 x y = x == y

-- Eq a => a -> a -> Bool
myTest5 :: Eq a => a -> a -> Bool
myTest5 x y = x == y
-- correct!

-- Eq a -> a -> a -> Bool
-- myTest6 :: Eq a -> a -> a -> Bool
-- myTest6 x y = x == y
-- error, Eq a is a typeclass not a type

-- Eq a => A -> Bool
-- myTest7 :: Eq a => A -> Bool
-- myTest7 x y = x == y
-- error, A not in scope

-- 3.

-- a.
myTest8 :: (a, b) -> a
myTest8 x = fst x
-- correct!

-- b.
-- myTest9 :: b -> a
-- myTest9 x = fst x
-- not working, can't mach b to expected type

-- c.
-- myTest10 :: (a, b) -> b
-- myTest10 x = fst x
-- nope, this would match to snd

-- 4.
-- d. is correct

myPlus2 :: Num a => a -> a -> a
myPlus2 x y = (+) x y
