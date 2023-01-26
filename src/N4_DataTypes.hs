{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
module N4_DataTypes where

import           Data.Char        (isDigit)
import           Data.Semigroup   as Semigroup (Semigroup ((<>)))
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Prelude          hiding (lookup)

data Color
  = Red
  | Green
  | Blue
  deriving (Show)

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel
  = Error
  | Warning
  | Info
  deriving (Show)

data LogEntry =
  LogEntry
    { timestamp :: UTCTime
    , logLevel  :: LogLevel
    , message   :: String
    }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString logEntry =
  (timeToString . timestamp) logEntry ++
  ": " ++ (show . logLevel) logEntry ++ ": " ++ message logEntry

abbrFirstName :: Person -> Person
abbrFirstName p
  | (length . firstName) p > 1 = p {firstName = (head . firstName) p : ['.']}
  | otherwise = p

data Coord a =
  Coord a a

-- distance :: Coord Double -> Coord Double -> Double
-- distance (Coord x1 y1) (Coord x2 y2) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2
manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x2 - x1) + abs (y2 - y1)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs)
  | isDigit x = Just x
  | otherwise = findDigit xs

-- findDigit = Data.List.find isDigit
--
-- findDigit ""
--
data Error
  = ParsingError
  | IncompleteDataError
  | IncorrectDataError String

data Person =
  Person
    { firstName :: String
    , lastName  :: String
    , age       :: Int
    }
  deriving (Show)

--   | label == "firstName" = person { firstName = value }
--   | label == "lastName" = person { lastName = value }
--   | label == "age" = person { age = read value }
-- person = Person {firstName="", lastName="", age=30}
-- parsePerson :: String -> Either Error Person
-- parsePerson personStr = map (correspondence . break (== '=')) (lines personStr)
--   where
--     correspondence (label, value) = (init label, (tail . tail) value)-- personFromStr (label, value)
--
-- parsePerson "firstName = John\nlastName = Connor\nage = 30"
data List a
  = Nil
  | Cons a (List a)

-- fromList :: List a -> [a]
-- fromList Nil         = []
-- fromList (Cons x xs) = x : fromList xs
toList :: [a] -> List a
toList []     = Nil
toList (x:xs) = Cons x (toList xs)

-- fromList (Cons 10 (Cons 1 Nil))
--
data Nat
  = Zero
  | Suc Nat

instance Show Nat where
  show :: Nat -> String
  show = show . fromNat

fromNat :: Nat -> Integer
fromNat Zero    = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n - 1))

add :: Nat -> Nat -> Nat
add Zero Zero       = Zero
add Zero y          = y
add x Zero          = x
add (Suc x) (Suc y) = Suc (Suc (add x y))

mul :: Nat -> Nat -> Nat
mul Zero Zero = Zero
mul Zero _    = Zero
mul x y       = toNat $ fromNat x * fromNat y

fac :: Nat -> Nat
fac Zero = Suc Zero
fac x    = toNat (product [1 .. (fromNat x)])

-- add (Suc (Suc (Suc (Suc (Suc Zero))))) (Suc (Suc (Suc (Suc (Suc Zero)))))
-- Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero)))))))))
--
-- mul (Suc (Suc Zero)) (Suc (Suc Zero))
-- Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero))))))))
--
-- fac (Suc (Suc (Suc (Suc Zero))))
--
data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _)   = 0
height (Node x y) = 1 + max (height x) (height y)

size :: Tree a -> Int
size (Leaf _)   = 1
size (Node x y) = 1 + size x + size y

sizeT :: Tree a -> Int
sizeT (Leaf _)   = 1
sizeT (Node x y) = sizeT x + sizeT y

sumT :: Tree Int -> Int
sumT (Leaf x)   = x
sumT (Node x y) = sumT x + sumT y

-- height (Node (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))))
--
avg :: Tree Int -> Int
avg t =
  let (c, s) = go t
   in s `div` c
  where
    go :: Tree Int -> (Int, Int)
    go x = (sizeT x, sumT x)

--
-- sizeT (Node (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Node (Leaf 1) (Leaf 1))) (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1))))
infixl 6 :+:

infixl 7 :*:

data Expr
  = Val Int
  | Expr :+: Expr
  | Expr :*: Expr
  deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) = expand e1 :*: expand e :+: expand e2 :*: expand e
expand (e :*: (e1 :+: e2)) = expand e :*: expand e1 :+: expand e :*: expand e2
expand (e1 :+: e2)         = expand e1 :+: expand e2
expand (e1 :*: e2)         = expand e1 :*: expand e2
expand e                   = e

-- expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)
-- Val 1 :*: Val 4 :+: (Val 1 :*: Val 5 :+: (Val 2 :*: Val 4 :+: (Val 2 :*: Val 5 :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5))))
newtype Xor =
  Xor
    { getXor :: Bool
    }
  deriving (Eq, Show)

instance Semigroup Xor where
  (<>) :: Xor -> Xor -> Xor
  Xor True <> Xor True   = Xor False
  Xor True <> Xor False  = Xor True
  Xor False <> Xor False = Xor False
  Xor False <> Xor True  = Xor True

instance Monoid Xor where
  mempty :: Xor
  mempty = Xor False
  mappend :: Xor -> Xor -> Xor
  mappend = (Semigroup.<>)

-- getXor $ mappend (Xor True) (Xor False)
--
newtype Maybe' a =
  Maybe'
    { getMaybe :: Maybe a
    }
  deriving (Eq, Show)

-- instance Semigroup a => Semigroup (Maybe' a)
--   -- (<>) :: Semigroup a => Maybe' a -> Maybe' a -> Maybe' a

-- instance Monoid a => Monoid (Maybe' a) where
--   mempty :: Monoid a => Maybe' a
--   mempty = Maybe' mempty
--   mappend :: Monoid a => Maybe' a -> Maybe' a -> Maybe' a
--   Maybe' a `mappend` Maybe' Nothing = Maybe' a
--   Maybe' Nothing `mappend` Maybe' b = Maybe' b
--   Maybe' a `mappend` Maybe' b       = Maybe' (a `mappend` b)

-- class MapLike m where
--   infixr 5 #
--   (#) :: Ord k => m k v -> m k v -> m k v
--   empty :: m k v
--   lookup :: Ord k => k -> m k v -> Maybe v
--   insert :: Ord k => k -> v -> m k v -> m k v
--   delete :: Ord k => k -> m k v -> m k v
--   fromList :: Ord k => [(k, v)] -> m k v
--   fromList = foldl (\acc (k, v) -> insert k v acc) empty
-- newtype ListMap k v = ListMap
--   { getListMap :: [(k, v)]
--   }
--   deriving (Eq, Show)
-- instance MapLike ListMap where
--   (#) (ListMap a) (ListMap b) = ListMap (b ++ a)
--   empty = ListMap []
--   lookup _ (ListMap []) = Nothing
--   lookup key (ListMap ((k, v) : kvs))
--     | key == k = Just v
--     | otherwise = lookup key (ListMap kvs)
--   insert key value (ListMap []) = ListMap [(key, value)]
--   insert key value (ListMap ((k, v) : kvs))
--     | key == k = ListMap $ (key, value) : kvs
--     | otherwise = (insert key value . ListMap) kvs # ListMap [(k, v)]
--   delete _ (ListMap []) = empty
--   delete key (ListMap ((k, v) : kvs))
--     | key == k = ListMap kvs
--     | otherwise =  (delete key . ListMap) kvs # ListMap [(k, v)]
-- --
-- list :: ListMap String Integer
-- delete "f" list
class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v
  fromList []          = empty
  fromList ((k, v):xs) = insert k v (fromList xs)

instance MapLike ArrowMap where
  empty :: ArrowMap k v
  empty = (ArrowMap . const) Nothing
  lookup :: Ord k => k -> ArrowMap k v -> Maybe v
  lookup key (ArrowMap f) = f key
  insert :: Ord k => k -> v -> ArrowMap k v -> ArrowMap k v
  insert key value (ArrowMap f) =
    ArrowMap
      (\k ->
         if k == key
           then Just value
           else f k)
  delete :: Ord k => k -> ArrowMap k v -> ArrowMap k v
  delete key (ArrowMap f) =
    ArrowMap
      (\k ->
         if k == key
           then Nothing
           else f k)

newtype ArrowMap k v =
  ArrowMap
    { getArrowMap :: k -> Maybe v
    }
--
