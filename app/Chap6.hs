module Chap6 where

import Data.List

-- Exercises: Eq Instances

-- 1.

data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn integer)
       (TisAn integer') =
    integer == integer'

-- 2.

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two integer1 integer2)
       (Two integer1' integer2') =
    integer1 == integer1' && integer2 == integer2'

 -- 3.

data StringOrInt =
    TisAnInt Int
  | TisAnString String
  deriving Show

instance Eq StringOrInt where
  (==) (TisAnInt int) (TisAnInt int') = int == int'
  (==) (TisAnString string) (TisAnString string') = string == string'
  (==) _ _ = False

-- 4.

data Pair a =
  Pair a a
  deriving Show

-- In this example I assume that Pair x y == Pair y x
instance Eq a => Eq (Pair a) where
  (==) (Pair x y)
       (Pair x' y') =
    (x == x' && y == y') ||
    (x == y' && y == x')

-- 5.

data Tuple a b =
  Tuple a b
  deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y)
       (Tuple x' y') =
    (x == x' && y == y')

-- 6.

data Which a =
    ThisOne a
  | ThatOne a
  deriving Show

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne y) (ThatOne y') = y == y'
  (==) _ _ = False

-- 7.

data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (Goodbye y) (Goodbye y') = y == y'
  (==) _ _ = False

  
-- Exercises: Will They Work?

willWork1 = max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])

willWork2 = compare (3 * 4) (3 * 5)

--willNotWork3 = compare "Julie" True

willWork4 = (5 + 3) > (3 + 6)

-- Chapter Exercies

-- Multiple choise

-- 1. c
-- 2. b
-- 3. a
-- 4. a
-- 5. a

-- Does it typecheck

-- 1.

-- Added derving Show so that printPerson works
--data Person = Person Bool
data Person = Person Bool deriving Show

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

-- 2., 3.

-- Added deriving Eq so that settleDown works
--data Mood = Blah
--          | Woot deriving ShowΩ

data Mood = Blah
          | Woot deriving (Show, Eq)

-- My extra task, I want Woot to be superior
instance Ord Mood where
  (<=) _ Woot  = True
  (<=) Blah Blah = True
  (<=) Woot Blah = False

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                  then Blah
                  else x

-- 4.
-- is fine

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?

data Rocks =
  Rocks String deriving (Eq, Show)

data Yeah =
  Yeah Bool deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- 1.
-- not working, as e.g. Rocks String /= String
-- phew = Papu "chaes" True
phew = Papu (Rocks "chaes") (Yeah True)

-- 2.
truth = Papu (Rocks "chomskydoz")
             (Yeah True)

-- 3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4.
-- Ord not derived/implemented, not working
--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'

-- Match the types

-- 1.
i1 :: Num a => a
i1 = 1


-- Not working, must implemented Num
--i1Alt :: a
--i1Alt = 1

-- 2., 3.
f2 :: Float
f2 = 1.0

-- Num not working, but we can use fractional
--f2Alt :: Num a => a
f2Alt :: Fractional a => a
f2Alt = 1.0

-- 4.
-- :info RealFrac
-- class (Real a, Fractional a) => RealFrac a whereΩ
f4Alt :: RealFrac a => a
f4Alt = 1.0

-- 5.
freud5 :: a -> a
freud5 x = x

-- Works but it is not needed:
--     • Redundant constraint: Ord a
--     • In the type signature for:
--           freud5Alt :: Ord a => a -> a
freud5Alt :: Ord a => a -> a
freud5Alt x = x

-- 6.
freud6Alt :: Int -> Int
freud6Alt x = x

-- 7.
myX = 1 :: Int

sigmund7 :: Int -> Int
sigmund7 x = myX

-- not working, as myX :: Int
--sigmund7Alt :: a -> a
--sigmund7Alt x = myX

-- 8.
-- not working, as myX :: Int
-- sigmund8Alt :: Num a => a -> a
-- sigmund8Alt x = myX

-- 9.
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

-- works, as Int implements Ord
jungAlt :: [Int] -> Int
jungAlt xs = head (sort xs)

-- 10.
young :: [Char] -> Char
young xs = head (sort xs)

youngAlt :: Ord a => [a] -> a
youngAlt xs = head (sort xs)

-- 11.
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

-- not working as, mySort :: [Char] -> [Char]
--signifierAlt :: Ord a => [a] -> a
--signifierAlt xs = head (mySort xs)

-- Type-Kwon-Do Two: Electric Typealoo

-- 1. noidea
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk = undefined

-- 2. noidea
arith :: Num b => (a -> b) -> Integer -> a -> b
arith = undefined

