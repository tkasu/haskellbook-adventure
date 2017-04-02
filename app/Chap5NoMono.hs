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

-- 4. Fractional a => a
e4f = 4 / e2y

-- 5. [Char] (String)
e5x = "Julie"
e5y = " <3 "
e5z = "Haskell"
e5f = e5x ++ e5y ++ e5z

-- Does it compile

-- 1. Not sure what is the correct one, but at least the beow solution works
-- bigNum = (^) 5 $ 10
bigNum = (^) 5
wahoo = bigNum $ 10

-- 2. works fine
c2x = print
c2y = print "woohoo!"
c3z = c2x "hello world"

-- 3. Changed c to be curried function
c3a = (+)
c3b = 5
--c3c = c3b 10
-- (+) ((+) 5 10)
c3c = c3a $ c3a c3b 10
c3d = c3c 200

--4. no idea
--c4a = 12 + c4b
--c4b = 10000 * c4c
--c4b = (*) 10000

-- Write correct type signature

-- 1.
functionH :: [a] -> a 
functionH (x:_) = x

-- 2.
functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3.
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
-- 1.
i :: a -> a
i x = x

-- 2.

c :: a -> b -> a
c x y = x

-- 3.
c'' :: b -> a -> b
c'' y x = y

-- 4.
c' :: a -> b -> b
c' x y = y

-- 5.
r :: [a] -> [a]
r x = reverse x

-- 6. TODO
co :: (b -> c) -> (a -> b) -> a -> c
co = undefined

-- 7. TODO
a :: (a -> c) -> a -> a
a = undefined

-- 8. TODO
a' :: (a -> b) -> a -> b
a' = undefined
