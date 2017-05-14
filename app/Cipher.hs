module Cipher where

import Data.Char

data Alpha = Alpha Char
  deriving (Eq, Show)

instance Bounded Alpha where
  minBound = Alpha 'a'
  maxBound = Alpha 'z'

instance Enum Alpha where
  toEnum x = Alpha (chr x)
  fromEnum (Alpha x) = ord x
  succ (Alpha x)
    | Alpha x == maxBound = minBound
    | otherwise = toEnum $ (fromEnum (Alpha x)) + 1
  pred (Alpha x)
    | Alpha x == minBound = maxBound
    | otherwise = toEnum $ (fromEnum (Alpha x)) - 1
