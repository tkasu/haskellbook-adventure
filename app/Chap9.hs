module Chap9 where

import Data.Char

-- Exercise: EnumFromTo
-- instead of doing etfBool, etfOrd, etfInt & etfChar I made myEtf that works for all of those.
-- also played a bit with Maybe, even though I'm not that familiar with it
myEtf ::  (Ord a, Enum a) => a -> a -> [a]
myEtf fst snd
  | fst >  snd = []
  | fst == snd = [fst]
  | fst <  snd = go fst snd Nothing
    where go next last prev = case prev of
            Just prevVal 
              | prevVal == last -> []
              | otherwise -> next : (go (succ next) last (Just next))
            Nothing -> next : (go (succ next) last (Just next))

-- Exercises: The Fearful Symmetry

testLine = "all i wanna do is have some fun"

-- 1.
myWords :: [Char] -> [[Char]]
myWords [] = []
myWords w
  | notSpace (head w) = (takeWhile notSpace w) : (myWords (dropWhile notSpace w))
  | isSpace  (head w) = myWords (dropWhile isSpace w)
       where notSpace = not . isSpace

-- 2.

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: [Char] -> [[Char]]
myLines [] = []
myLines w
  | notNewLine (head w) = (takeWhile notNewLine w) : (myLines (dropWhile notNewLine w))
  | isNewLine  (head w) = myLines (dropWhile isNewLine w)
       where isNewLine = (== '\n')
             notNewLine = not . isNewLine

-- 3.
mySplitter :: Eq a => a -> [a] -> [[a]]
mySplitter _ [] = []
mySplitter e w
  | notSplitElem (head w) = (takeWhile notSplitElem w) : (mySplitter e (dropWhile notSplitElem w))
  | isSplitElem  (head w) = mySplitter e (dropWhile isSplitElem w)
       where isSplitElem = (== e)
             notSplitElem = not . isSplitElem

myWords' w = mySplitter ' ' w

myLines' w = mySplitter '\n' w

-- Exercises: Comprehend Thy Lists

mySqr = [x^2 | x <- [1..5]]

--- only even numbers
what1 = [x | x <- mySqr, rem x 2 == 0]

-- empty list, as no y satisfies the filter condition
what2 = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

-- empty list, as no y satisfies the filter condition
what3 = take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

-- Exercises: Square Cube

myCube = [y^3 | y <- [1..5]]

-- 1.
sqrsAndCubes = [(x, y) | x <- mySqr, y <- myCube]

-- 2.
sqrsAndCubesFilt1 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3.
sqrsAndCubesFilt2 = take 4 [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]


-- Exercises: Bottom Madness

-- will blow
blow1 = [x^y | x <- [1..5], y <- [2, undefined]]

-- will not
blow2 = take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

-- will blow
blow3 = sum [1, undefined, 3]

-- will not (only uses the spine)
blow4 = length [1, 2, undefined]

-- will blow
blow5 = length $ [1, 2, 3] ++ undefined

-- will not (wrong guess)
blow6 = take 1 $ filter even [1, 2, 3, undefined]

-- will blow this time!
blow7 = take 1 $ filter even [1, 3, undefined]

-- will not
blow8 = take 1 $ filter odd [1, 3, undefined]

-- will not
blow9 = take 2 $ filter odd [1, 3, undefined]

-- will blow
blow10 = take 3 $ filter odd [1, 3, undefined]

-- Exercises: More Bottoms

-- will blow
blow11 = take 1 $ map (+1) [undefined, 2, 3]

-- will not
blow12 = take 1 $ map (+1) [1, undefined, 3]

-- will blow
blow13 = take 2 $ map (+1) [1, undefined, 3]

-- 4.
itIsMystery :: [Char] -> [Bool]--
itIsMystery xs = map (\x -> elem x "aeiou") xs

-- 5.

whatA = map (^2) [1..10]
-- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100

whatB = map minimum [[1..10], [10..20], [20..30]]
-- [1, 10, 20]

whatC = map sum [[1..5], [1..5], [1..5]]
-- [15, 15, 15]

-- Exercises: Filtering

-- 1.
mult3 = filter (\x -> rem x 3 == 0) [1..30]

-- 2.
cntMult3 = length $ filter (\x -> rem x 3 == 0) [1..30]

-- 3.

toFilter = "the brown dog was a goof"

myFilter :: [Char] -> [[Char]]
myFilter s = filter notArticle ws
  where isThe = \w -> (==) "the" w || (==) "The" w
        isA   = \w -> (==) "a" w || (==) "A" w
        isAn  = \w -> (==) "an" w || (==) "An" w
        isArticle = \w -> (isThe w) || (isA w) || (isAn w)
        notArticle = not . isArticle
        ws = words s

-- Zipping exercises

-- 1.
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : (myZip xs ys)

-- 2.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y) : (myZipWith f xs ys)

-- 3.
myZip' xl yl = myZipWith (\x y -> (x, y)) xl yl

-- Chapter Exercises

-- Data.Char

-- 1.
-- isUpper :: Char -> Bool 	-- Defined in ‘GHC.Unicode’
-- toUpper :: Char -> Char 	-- Defined in ‘GHC.Unicode’

-- 2.
badHello = "HbEfLrLxO,"

filtUpper :: [Char] -> [Char]
filtUpper s = [x | x <- s, isUpper x]

-- 3.
capFirst :: [Char] -> [Char]
capFirst [] = []
capFirst (x:xs) = toUpper x : xs

-- 4.
capAll :: [Char] -> [Char]
capAll [] = []
capAll (x:xs) = toUpper x : capAll xs

-- 5.
capHead :: [Char] -> Char
capHead l = toUpper . head $ l

-- 6.
capHead' :: [Char] -> Char
capHead' = toUpper . head

-- Ciphers in module Cipher.hs


-- Writing your own standard functions

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = if x == False then False else myAnd xs

-- 1.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs 

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem e l = myAny (== e) l

-- 4.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse l = go l []
  where go [] acc     = acc
        go (x:xs) acc = go xs ([x] ++ acc)
