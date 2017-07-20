module Chap10 where

import Data.Time 


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

-- Exercises: Database Processing

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem] 
theDatabase =
    [DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

-- 1.
-- foldr may not be optimal for this (maybe filter?), but this is recursion chapter after all
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = foldr f [] db
    where f next acc =
            case next of
              DbDate x -> [x] ++ acc
              x -> [] ++ acc

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr f [] db
    where f next acc =
            case next of
              DbNumber x -> [x] ++ acc
              x -> [] ++ acc

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime 
mostRecent db = maximum $ filterDbDate db

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb db = sum $ filterDbNumber db

mixedIdentityDatabase :: [DatabaseItem]
mixedIdentityDatabase = DbNumber 1234 : theDatabase

testSumDb :: Integer
testSumDb = sumDb mixedIdentityDatabase

-- 5.
countDbNums :: [DatabaseItem] -> Int
countDbNums db = length $ filterDbNumber db

avgDb :: [DatabaseItem] -> Double 
avgDb db = (fromIntegral $ sumDb db) / (fromIntegral $ countDbNums db) 

-- Scan exericese
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- 1.
fibsFirstN :: Int -> [Integer]
fibsFirstN n = take n fibs

fibsFirst20 = fibsFirstN 20

-- 2.
-- first try, but not a good one as it goes on and on
fibsLessThanN :: Integer -> [Integer]
fibsLessThanN n = [x | x <- fibs, x < n]

-- second try, same problem than with first try
fibsLessThanN' :: Integer -> [Integer]
fibsLessThanN' n = foldr f [] fibs
                where f next acc 
                        | next < n  = next : acc
                        | otherwise = acc

-- better and simpler
fibsLessThanN'' :: Integer -> [Integer]
fibsLessThanN'' n = takeWhile (\x -> x < n) fibs
                
fibsLessThan100 = fibsLessThanN'' 100

-- 3.
-- first without scan
factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- with scan
factorialSeries :: [Integer]
factorialSeries = scanl (*) 1 [2..]

factorial' :: Int -> Integer
factorial' n = factorialSeries !! (n - 1)


-- Chapter Exerceis

-- Warm up and review

-- 1.
stops  = "pbtdkg"
vowels = "aeiou"

-- a.
allCombinationsXYX :: [a] -> [b] -> [(a, b, a)]
allCombinationsXYX [] yl = []
allCombinationsXYX xl [] = []
allCombinationsXYX xl yl = [(x, y, z) | x <- xl, y <- yl, z <- xl]

-- b.
combsXYXThat :: [a] -> [b] -> ((a, b, a) -> Bool) -> [(a, b, a)]
combsXYXThat [] yl pred = []
combsXYXThat xl [] pred = []
combsXYXThat xl yl pred = filter pred $ allCombinationsXYX xl yl

stopVowelsStartWithP :: [(Char, Char, Char)]
stopVowelsStartWithP = combsXYXThat stops vowels isP
                    where isP (x, _, _) = (==) x 'p'

-- c.
nouns = ["people", "history", "way", "art", "world"]
verbs = ["were", "had", "did", "said", "went"]

nounsVerbsCombs :: [(String, String, String)]
nounsVerbsCombs = allCombinationsXYX nouns verbs

-- 2. 
-- calculates average length of a word in a text
seekritFunc :: String -> Int
seekritFunc x =
    div (sum (map length (words x)))
        (length (words x))

-- 3.
seekritFuncAcc :: String -> Double
seekritFuncAcc x =
    (/) (fromIntegral $ sum (map length (words x)))
        (fromIntegral $ length (words x))


-- Rewriting functions using folds

myOr :: [Bool] -> Bool 
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool 
myAny f xs = foldr fChain False xs
    where fChain = \next acc -> (f next) || acc

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = myAny ((==) x) xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs = foldr (\next acc -> ((==) x next) || acc) False xs

myReverse :: [a] -> [a] 
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr fChain [] xs
    where fChain = \next acc -> (f next) : acc

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr fChain [] xs
    where fChain = \next acc -> case (f next) of
                                    True -> next : acc
                                    False -> acc

squish :: [[a]] -> [a] 
squish = foldr f []
    where f = \next acc -> foldr (:) acc next

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr fChain [] xs
    where fChain = \next acc -> (f next) ++ acc

--maybe later, too much to ask atm
squishAgain :: [[a]] -> [a] 
squishAgain = undefined 

-- This is only working with compare, I don't fully understand to purpose of myMaximumBy (\_ _ -> GT) [1..10] = 1
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a 
myMaximumBy f (x:xs) = foldr fChain x xs
        where fChain = \next acc -> case (f next acc) of 
                                        GT -> next
                                        LT -> acc
                                        EQ -> acc

-- Same as above, only works with compare
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a 
myMinimumBy f (x:xs) = foldr fChain x xs
        where fChain = \next acc -> case (f next acc) of 
                                        LT -> next
                                        GT -> acc
                                        EQ -> acc