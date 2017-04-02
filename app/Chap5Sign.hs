module Chp5Sing where

--fstString :: [Char] ++ [Char]
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

--sndString :: [Char] -> Char
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

--sing = if (x > y) then fstString x or sndString y
sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        --x = "Somewhere"
        y = "Somewhere"

singOther = if (x <= y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"
