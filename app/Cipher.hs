module Cipher where

import Data.Char

data Alpha = Alpha Char
  deriving (Eq, Show)

instance Enum Alpha where
  toEnum x = Alpha (chr x)
  fromEnum (Alpha x) = ord x
  succ (Alpha 'z') = Alpha 'a'
  succ (Alpha 'Z') = Alpha 'A'
  succ (Alpha c)   = Alpha (succ c)
  pred (Alpha 'a') = Alpha 'z'
  pred (Alpha 'A') = Alpha 'Z'
  pred (Alpha c)   = Alpha (pred c)

toAlpha :: Char -> Alpha
toAlpha c
  | elem c ['a'..'z'] = Alpha c
  | elem c ['A'..'Z'] = Alpha c

fromAlpha :: Alpha -> Char
fromAlpha (Alpha c) = c

toAlphaSeq :: [Char] -> [Alpha]
toAlphaSeq [] = []
toAlphaSeq (x:xs) = toAlpha x : toAlphaSeq xs

fromAlphaSeq :: [Alpha] -> [Char]
fromAlphaSeq [] = []
fromAlphaSeq (x:xs) = fromAlpha x : fromAlphaSeq xs

applyTimes :: Integer -> (a -> a) -> a -> a
applyTimes 0 f x = x
applyTimes n f x = applyTimes (n - 1) f (f x)

rotateAlphaSeq :: Integer -> [Alpha] -> [Alpha]
rotateAlphaSeq _ [] = []
rotateAlphaSeq 0 xs = xs  
rotateAlphaSeq n (x:xs)
  | n > 0 = (applyTimes n succ x) : (rotateAlphaSeq n xs)
  | n < 0 = (applyTimes (abs n) pred x) : (rotateAlphaSeq n xs)

-- Will only work if all items in seq are in [a..z] ++ [A..Z]
-- Otherwise the program will crash (will be upgraded to use Maybe afterwards)
ceasar :: Integer -> [Char] -> [Char]
ceasar n xs = fromAlphaSeq . (rotateAlphaSeq n) . toAlphaSeq $ xs

unCeasar :: Integer -> [Char] -> [Char]
unCeasar n xs = fromAlphaSeq . (rotateAlphaSeq (-n)) . toAlphaSeq $ xs

