{-# LANGUAGE FlexibleInstances #-}

module Chap11 where

import Data.Int


--  Exercises: Dog types

data PugType = PugData 
data HuskyType a = HuskyData 
data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = 
    Husky a
    | Mastiff a 
    deriving (Eq, Show)

-- 1. Doggies is a type constructor
-- 2. :k Doggies -> Doggies :: * -> *
-- 3. :k (Doggies String) -> (Doggies String) :: *
-- 4. :t Husky 10 -> Husky 10 :: Num a => Doggies a
-- 5. :t Husky (10 :: Integer) -> Husky (10 :: Integer) :: Doggies Integer
-- 6. :t Mastiff "Scooby Doo" Mastiff "Scooby Doo" :: Doggies [Char]
-- 7. In this case both, as in  "data DogueDeBordeaux a = DogueDeBordeaux a" type constructor and data constructor have equal name
-- 8. :t DogueDeBordeaux -> DogueDeBordeaux :: doge -> DogueDeBordeaux doge
-- 9. :t DogueDeBordeaux "doggie!" -> DogueDeBordeaux "doggie!" :: DogueDeBordeaux [Char]


-- Exercises: Vehicles

data Price = Price Integer 
    deriving (Eq, Show)

data Manufacturer = 
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline = 
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle = 
    Car Manufacturer Price
    | Plane Airline
    deriving (Eq, Show)

myCar = Car Mini (Price 14000) 
urCar = Car Mazda (Price 20000) 
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. myCar :: Vechicle

-- 2. 
isCar :: Vehicle -> Bool 
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool 
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool] 
areCars = map isCar

-- 3.
getManu :: Vehicle -> Manufacturer 
getManu (Car m _) = m

-- 4.
willThrowException = getManu doge

-- 5.
data Size = Size Integer 
    deriving (Eq, Show)

data NewVehicle = 
    NewCar Manufacturer Price
    | NewPlane Airline Size
    deriving (Eq, Show)

newDoge = NewPlane PapuAir (Size 100)

isNewPlane :: NewVehicle -> Bool 
isNewPlane (NewPlane _ _) = True
isNewPlane _              = False


-- Excersices: Cardinality
-- 1. PugType cardinality 1
-- 2. Airline cardinality 3
-- 3. 
int8cardinality :: Integer
int8cardinality = int8max + (-int8min) + 1
    where int8min = fromIntegral $ (minBound :: Int8)
          int8max = fromIntegral $ (maxBound :: Int8)
-- -> 256

int16cardinality :: Integer
int16cardinality = int16max + (-int16min) + 1
    where int16min = fromIntegral $ (minBound :: Int16)
          int16max = fromIntegral $ (maxBound :: Int16)
-- -> 65536

-- 4.
intCardinality :: Integer
intCardinality = intMax + (-intMin) + 1
    where intMin = fromIntegral $ (minBound :: Int)
          intMax = fromIntegral $ (maxBound :: Int)
-- -> 18446744073709551616

-- Integer has not implemented Bounded as by implementation it is arbitrary precision? There should never be Int overflows.


-- Exercises: For Example
data Example = MakeExample deriving Show

-- 1. 
-- :t MakeExample -> MakeExample :: Example
-- :t Example -> error: Data constructor not in scope: Example

-- 2. :info Example
-- data Example = MakeExample
-- instance [safe] Show Example

-- 3.
data Example2 = Example2 Int deriving Show
-- :t Example2 -> Example2 :: Int -> Example2


--  Exercies: Logic Goats
class TooMany a where 
    tooMany :: a -> Bool

instance TooMany Int where 
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)

instance TooMany Goats where 
    tooMany (Goats n) = tooMany n

-- requires FlexibleInstances
instance TooMany (Int, String) where
    tooMany (n, _) = tooMany n

-- requires FlexibleInstances
instance TooMany (Int, Int) where
    tooMany (n1, n2) = tooMany (n1 + n2)

-- TODO
-- requires FlexibleInstances
--instance TooMany (Num a, TooMany a) where
--    tooMany (n1, n2) = tooMany (n1 + n2)


-- Exercises: Pity the Bool
-- TODO


-- Exercises: How Does the Garden Grow?
-- TODO


-- Exercise: Programmer
data OperatingSystem =
        GnuPlusLinux
        | OpenBSDPlusNevermindJustBSDStill 
        | Mac
        | Windows
        deriving (Eq, Show)

data ProgrammingLanguage =
        Haskell
        | Agda
        | Idris
        | PureScript 
        deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
                , lang :: ProgrammingLanguage } 
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer] 
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

programmerCountCheck :: Bool
programmerCountCheck = (length allProgrammers) == (length allOperatingSystems * length allLanguages)


-- Exericises: Binary Tree
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a 
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right 
    | b > a  = Node left a (insert' b right)

-- implement map for BinaryTree
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf) 
    
mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected 
    then print "yup okay!"
    else error "test failed!"

-- implement BinaryTree to Lists
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
    [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
    (preorder left) ++ [a] ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) =
    (preorder left) ++ (preorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO () 
testPreorder =
    if preorder testTree == [2, 1, 3] 
    then putStrLn "Preorder fine!" 
    else putStrLn "Bad news bears."

testInorder :: IO () 
testInorder =
    if inorder testTree == [1, 2, 3] 
    then putStrLn "Inorder fine!" 
    else putStrLn "Bad news bears."

testPostorder :: IO () 
testPostorder =
    if postorder testTree == [1, 3, 2] 
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

--main :: IO () main = do
-- mockMain function to test BinaryTree a -> [a] functions
mockMain :: IO ()
mockMain = do
  testPreorder
  testInorder
  testPostorder

-- implement fold for BinaryTree a (any traversal order)
-- this might be a bit cheating?
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f z bTree = foldr f z (inorder bTree) 