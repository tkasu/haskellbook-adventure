module Reverse where

-- Chapter 3 exercise

dropLast :: [a] -> [a]
dropLast x  = take (len - 1) x
  where len = length x

rvrs :: [a] -> [a]
rvrs []      = []
rvrs x       = last : rvrs (dropLast x)
  where len  = length x
        last = x !! (len - 1)

main :: IO ()
main = print $ rvrs "Curry is awesome."
