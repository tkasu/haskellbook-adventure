module Chap7Arith4 where

--id :: a -> a
--id x = x
  
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5.
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- 6.
-- Probably not be what the exercise is looking for 
roundTrip'' :: (Show a, Read b, Integral b) => a -> b
roundTrip'' = read . show

main = do
  print (roundTrip 4)
  print (id 4)
  print (roundTrip' 4)
  print (roundTrip'' 4)
