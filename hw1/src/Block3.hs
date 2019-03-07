{-# LANGUAGE InstanceSigs #-}

module Block3
  (
    Day(..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay

  , City(..)
  , citizens
  , cityLord
  , cityWalls

  , Castle(..)
  , House(..)
  , Landmark(..)
  , LivingPersons(..)
  , Lord(..)
  , Walls(..)
  , buildCastle
  , buildHouse
  , buildLandmark
  , buildWalls
  , setLord

  , OperationResult(..)
  , getResult
  , showResult

  , Nat(..)
  , (+)
  , (-)
  , (*)
  , (==)
  , compare
  , div
  , fromInteger
  , mod
  , toInteger
  , Prelude.even

  , Tree(..)
  , checkFoldable
  , find
  , Block3.fromList
  , insert
  , isEmpty
  , remove
  , size
  ) where

import Prelude

import Data.List.NonEmpty as NE ((<|), head, map, fromList, NonEmpty((:|)))
import Data.List (sort)
import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Ratio ((%))
import Data.Foldable (toList)

-- First task

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show)

instance Enum Day where
  toEnum :: Int -> Day
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
  toEnum x = toEnum (x `mod` 7)

  fromEnum :: Day -> Int
  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

nextDay :: Day -> Day
nextDay = succ

afterDays :: Int -> Day -> Day
afterDays x day = toEnum $ fromEnum day + x

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

daysToParty :: Day -> Int
daysToParty day = (fromEnum Friday - fromEnum day + 7) `mod` 7

-- /First task

-- Second task

data City = City
  { castle :: Maybe Castle
  , landmark :: Maybe Landmark
  , houses :: NonEmpty House
  } deriving (Show)

data Landmark = Church | Library
  deriving (Show)

data Lord = Lord
  deriving (Show)

data Walls = Walls
  deriving (Show)

data Castle = Castle
  { lord  :: Maybe Lord
  , walls :: Maybe Walls
  } deriving (Show)

cityWalls :: City -> Maybe Walls
cityWalls city = walls $ fromMaybe emptyCastle (castle city)

cityLord :: City -> Maybe Lord
cityLord city = lord $ fromMaybe emptyCastle (castle city)

citizens :: City -> Int
citizens city = sum $ NE.map (fromEnum . livingPersons) $ houses city

emptyCastle :: Castle
emptyCastle = Castle Nothing Nothing

newtype House = House { livingPersons :: LivingPersons }
  deriving (Show)

data LivingPersons = One | Two | Three | Four
  deriving (Show)

instance Enum LivingPersons where
  toEnum :: Int -> LivingPersons
  toEnum 1 = One
  toEnum 2 = Two
  toEnum 3 = Three
  toEnum 4 = Four
  toEnum _ = error "Wrong number of people"

  fromEnum :: LivingPersons -> Int
  fromEnum One   = 1
  fromEnum Two   = 2
  fromEnum Three = 3
  fromEnum Four  = 4

data OperationResult = Success (City, String) | Failure (City, String)

showResult :: OperationResult -> String
showResult (Success x) = "Success: " ++ snd x
showResult (Failure x) = "Failure: " ++ snd x

getResult :: OperationResult -> City
getResult (Success x) = fst x
getResult (Failure x) = fst x

buildCastle :: City -> OperationResult
buildCastle city
  | isNothing $ castle city = Success (city { castle = Just emptyCastle }, "castle built")
  | otherwise               = Failure (city, "city already has castle")

buildLandmark :: City -> Landmark -> OperationResult
buildLandmark city land
  | isNothing $ landmark city = Success (city { landmark = Just land }, show land ++ " built")
  | otherwise                 = Failure (city, "city already has " ++ show (fromJust $ landmark city))

buildHouse :: City -> LivingPersons -> OperationResult
buildHouse city persons = Success (city { houses = House persons <| houses city }, "new house was built")

setLord :: City -> OperationResult
setLord city
  | isNothing $ castle city   = Failure (city, "there is no castle for lord")
  | isNothing $ cityLord city = Success (city { castle = Just $ Castle (Just Lord) (cityWalls city) }, "lord was set")
  | otherwise                 = Failure (city, "castle already has lord")

buildWalls :: City -> OperationResult
buildWalls city
  | isNothing $ cityLord city  = Failure (city, "there is no lord")
  | citizens city < 10         = Failure (city, "there is not enough people")
  | isNothing $ cityWalls city = Success (city { castle = Just $ Castle (cityLord city) (Just Walls) }, "walls were built")
  | otherwise                  = Failure (city, "city already has walls")

-- /Second task

-- Third task

data Nat = Z | S Nat
  deriving (Show)

instance Enum Nat where
  toEnum :: Int -> Nat
  toEnum a = fromInteger $ toInteger a

  fromEnum :: Nat -> Int
  fromEnum a = fromInteger $ toInteger a

  succ :: Nat -> Nat
  succ = S

  pred :: Nat -> Nat
  pred (S a) = a
  pred Z     = Z

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) a Z = a
  (+) a b = (+) (succ a) (pred b)

  (-) :: Nat -> Nat -> Nat
  (-) a Z = a
  (-) a b = (-) (pred a) (pred b)

  (*) :: Nat -> Nat -> Nat
  (*) _ Z = Z
  (*) Z _ = Z
  (*) a b = iter a a b
    where
      iter _ x (S Z) = x
      iter c x y     = iter c (x + c) (pred y)

  abs :: Nat -> Nat
  abs a = a

  signum :: Nat -> Nat
  signum Z = Z
  signum _ = S Z

  fromInteger :: Integer -> Nat
  fromInteger a | a <= 0    = Z
                | otherwise = iter a Z
                    where
                      iter 0 nat   = nat
                      iter int nat = iter (int - 1) (succ nat)

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) a b = compare a b == EQ

instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  compare Z Z = EQ
  compare _ Z = GT
  compare Z _ = LT
  compare a b = compare (pred a) (pred b)

instance Real Nat where
  toRational a = toInteger a % 1

instance Integral Nat where
  toInteger :: Nat -> Integer
  toInteger a = iter a 0
    where
      iter Z int   = int
      iter nat int = iter (pred nat) (int + 1)

  quotRem :: Nat -> Nat -> (Nat, Nat)
  quotRem _ Z = error "division by zero"
  quotRem a b = iter a b Z
    where
      iter x y i
        | x < y     = (i, x)
        | otherwise = iter (x - y) y (succ i)

  divMod :: Nat -> Nat -> (Nat, Nat)
  divMod = quotRem

-- even is in Prelude for Integral

-- /Third task

-- Fourth task

data Tree a = Leaf | Node (NonEmpty a) (Tree a) (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf                   = mempty
  foldMap f (Node list left right) = foldMap f left `mappend` foldMap f list `mappend` foldMap f right

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf                   = z
  foldr f z (Node list left right) = foldr f (foldr f (foldr f z right) list) left


isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree a -> Int
size Leaf                   = 0
size (Node list left right) = length list + size left + size right

find :: Ord a => a -> Tree a -> Bool
find _ Leaf           = False
find el (Node list left right)
  | NE.head list > el = find el left
  | NE.head list < el = find el right
  | otherwise         = True

insert :: Ord a => a -> Tree a -> Tree a
insert el Leaf        = Node (el :| []) Leaf Leaf
insert el (Node list left right)
  | NE.head list > el = Node list (insert el left) right
  | NE.head list < el = Node list left (insert el right)
  | otherwise         = Node (el <| list) left right

remove :: Ord a => a -> Tree a -> Tree a
remove _ Leaf  = Leaf
remove el (Node (x:|xs) left right)
  | x > el     = Node (x:|xs) (remove el left) right
  | x < el     = Node (x:|xs) left (remove el right)
  | otherwise  = if xs /= []
                 then Node (NE.fromList xs) left right
                 else delete left right
                   where
                     delete :: Ord a => Tree a -> Tree a -> Tree a
                     delete Leaf r = r
                     delete l Leaf = l
                     delete l r    = let minList = minEl r in
                                       Node minList l (fullRemove minList r)

                     minEl :: Ord a => Tree a -> NonEmpty a
                     minEl (Node list l' _) =
                       case l' of
                         Leaf -> list
                         node -> minEl node
                     minEl Leaf = error "unreachable statement"


                     fullRemove :: Ord a => NonEmpty a -> Tree a -> Tree a
                     fullRemove (y:|[]) tree' = remove y tree'
                     fullRemove (y:|ys) tree' = fullRemove (NE.fromList ys) (remove y tree')

 -- fromList from Block4 (hard)
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert Leaf

checkFoldable :: Ord a => [a] -> Bool
checkFoldable list = (toList . Block3.fromList) list == sort list

-- /Fourth task
