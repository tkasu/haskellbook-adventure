module Chap9 where

import Data.Char (isSpace)

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

