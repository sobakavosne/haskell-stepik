{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TupleSections #-}

module Part_1.N5_Monads where

import           Control.Monad       (foldM, liftM)
import           Control.Monad.State (replicateM_)
import           Data.Bifunctor      (Bifunctor (bimap, second))
import           Data.Char           (isDigit, toUpper)
import           Data.Functor        ((<&>))
import           Data.List           (isInfixOf)
import           Data.Semigroup      (Sum (Sum))
import           System.Directory    (getDirectoryContents, removeFile)

data Point3D a =
  Point3D a a a
  deriving (Show)

instance Functor Point3D where
  fmap :: (a -> b) -> Point3D a -> Point3D b
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a
  = Point (Point3D a)
  | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
  fmap :: (a -> b) -> GeomPrimitive a -> GeomPrimitive b
  fmap f (Point (Point3D x y z)) = Point $ Point3D (f x) (f y) (f z)
  fmap f (LineSegment a b)       = LineSegment (fmap f a) (fmap f b)

data Tree' a
  = Leaf' (Maybe a)
  | Branch (Tree' a) (Maybe a) (Tree' a)
  deriving (Show)

instance Functor Tree' where
  fmap :: (a -> b) -> Tree' a -> Tree' b
  fmap f (Leaf' a)      = Leaf' (fmap f a)
  fmap f (Branch a b c) = Branch (fmap f a) (fmap f b) (fmap f c)

data Entry k1 k2 v =
  Entry (k1, k2) v
  deriving (Show)

newtype Map k1 k2 v =
  Map [Entry k1 k2 v]
  deriving (Show)

instance Functor (Entry k1 k2) where
  fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)

instance Functor (Map k1 k2) where
  fmap f (Map x) = Map (map (fmap f) x)

-- fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
--
data Log a =
  Log [String] a
  deriving (Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x log1 log2 =
  case log1 x of
    Log msg1 a ->
      case log2 a of
        Log msg2 b -> Log (msg1 ++ msg2) b

add1Log :: Integer -> Log Integer
add1Log = toLogger (+ 1) "added one"

mult2Log :: Integer -> Log Integer
mult2Log = toLogger (* 2) "multiplied by 2"

-- execLoggers 3 add1Log mult2Log
--
toKleisli :: Monad m => (a -> b) -> a -> m b
toKleisli f = return . f

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg1 a) f = Log (msg1 ++ msg2) b
  where
    Log msg2 b = f a

-- Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
--
instance Functor Log where
  fmap :: (a -> b) -> Log a -> Log b
  fmap = liftM

instance Applicative Log where
  pure :: a -> Log a
  pure = returnLog
  (<*>) :: Log (a -> b) -> Log a -> Log b
  (<*>) = undefined

instance Monad Log where
  return :: a -> Log a
  return = pure
  (>>=) :: Log a -> (a -> Log b) -> Log b
  (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return

-- execLoggersList a [] = return a
-- execLoggersList ini (f:fs) = f ini >>= (`execLoggersList` fs)
--
-- execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
-- return 3 >>= (\x -> add1Log x >>= mult2Log)
-- return 3 >>= add1Log
-- add1Log 3 >>= return
-- add1Log 3
newtype SomeType a =
  SomeType a

instance Monad SomeType where
  return :: a -> SomeType a
  return = pure
  (>>=) :: SomeType a -> (a -> SomeType b) -> SomeType b
  (>>=) (SomeType a) f = f a

instance Applicative SomeType where
  pure :: a -> SomeType a
  pure = SomeType
  (<*>) :: SomeType (a -> b) -> SomeType a -> SomeType b
  (<*>) = undefined

instance Functor SomeType where
  fmap :: (a -> b) -> SomeType a -> SomeType b
  fmap f x = x <&> f

{-
  Monad laws:
    1. return x >>= f ≡ f x
    where f is Kleisli arrow

    2. m >>= return is equal to m

    3. (m >>= f) >>= g is equal to m >>= (\x -> f x >>= g)
                     ~ is equal to m >>= \x -> f x >>= g
-}
data Token
  = Number Int
  | Plus
  | Minus
  | LeftBrace
  | RightBrace
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken x
  | all isDigit x = (Just . Number . read) x
  | x == "+" = Just Plus
  | x == "-" = Just Minus
  | x == "(" = Just LeftBrace
  | x == ")" = Just RightBrace
  | otherwise = Nothing

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words

-- tokenize "1 + ( 7 - 2 )"
-- mapM (\x -> [x]) [1,2]
--
-- pythagoreanTriple :: Int -> [(Int, Int, Int)]
-- pythagoreanTriple x =
--   [ (a, b, c)
--   | b <- [1 ..100]
--   , a <- [1 .. b]
--   , c <- [1 .. x]
--   , a ^ 2 + b ^ 2 == c ^ 2
--   ]
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do
  c <- [1 .. x]
  b <- [1 .. (c - 1)]
  a <- [1 .. (b - 1)]
  True <- return $ a ^ 2 + b ^ 2 == c ^ 2
  return (a, b, c)

-- pythagoreanTriple 5
--
main' :: IO ()
main' = do
  putStr "What is your name?\nName: "
  name <- getLine
  case name of
    "" -> main'
    _  -> putStrLn ("Hi, " ++ name ++ "!")

-- nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
-- nextPositionsN b n pred
--   | n == 0 = [b | pred b]
--   | n < 0 = []
--   | otherwise =
--     concat [nextPositionsN b' (n - 1) pred | b' <- nextPositions b, pred b']
--
main'' :: IO ()
main'' = do
  putStr "Substring: "
  substring <- getLine
  case substring of
    "" -> putStrLn "Canceled"
    _ -> do
      contents <- getDirectoryContents "."
      let matchedFiles = filter (isInfixOf substring) contents
      mapM_
        (\file -> putStrLn ("Removing file: " ++ file) >> removeFile file)
        matchedFiles

safeHead :: [a] -> Maybe a
safeHead = do
  b <- null
  if b
    then return Nothing
    else do
      Just <$> head

newtype Reader r a =
  Reader
    { runReader :: r -> a
    }

firstUser :: Reader [(a1, a2)] a2
firstUser = Reader (snd . head)

local :: (r -> b) -> Reader b a -> Reader r a
local f m = Reader (runReader m . f)

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f x = x <&> f

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = Reader . const
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) = undefined

instance Monad (Reader r) where
  return :: a -> Reader r a
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader (runReader m . f)

type User = String

type Password = String

type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords =
  Reader
    (foldr
       (\(user, password) acc ->
          case password of
            "123456" -> user : acc
            _        -> acc)
       [])

newtype Writer w a =
  Writer
    { runWriter :: (a, w)
    }

writer :: (a, w) -> Writer w a
writer = Writer

instance Functor (Writer w) where
  fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f x = x <&> f

instance Semigroup (Writer w a) where
  (<>) :: Writer w a -> Writer w a -> Writer w a
  (<>) = undefined

instance Monoid w => Applicative (Writer w) where
  pure :: Monoid w => a -> Writer w a
  pure x = Writer (x, mempty)
  (<*>) :: Monoid w => Writer w (a -> b) -> Writer w a -> Writer w b
  (<*>) = undefined

instance (Monoid a, Monoid w) => Monoid (Writer w a) where
  mempty :: (Monoid a, Monoid w) => Writer w a
  mempty = pure mempty
  mappend :: (Monoid a, Monoid w) => Writer w a -> Writer w a -> Writer w a
  mappend = (<>)

instance Monoid w => Monad (Writer w) where
  return :: Monoid w => a -> Writer w a
  return = pure
  (>>=) :: Monoid w => Writer w a -> (a -> Writer w b) -> Writer w b
  m >>= k = Writer (y, u <> v)
    where
      (x, u) = runWriter m
      (y, v) = runWriter (k x)

tell :: w -> Writer w ()
tell w = Writer ((), w)

execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

evalWriter :: Writer w a -> a
evalWriter m = fst (runWriter m)

-- runWriter $ writer (11, "A") >>= \x -> writer (1 + x, "B") >>= \y -> writer (y,"C")
--
-- newtype Sum a =
--   Sum a
--
type Shopping = Writer (Sum (Integer, [String])) ()

-- instance Show a => Show (Sum a) where
--   show (Sum a) = show a
-- instance Num a => Semigroup (Sum a) where
--   (<>) (Sum a) (Sum b) = Sum (fst a + fst b)
-- instance Num a => Monoid (Sum a) where
--   mempty = Sum 0
--   mappend = (<>)
-- instance Functor Sum where
--   fmap f (Sum a) = Sum (f a)
-- instance Applicative Sum where
--   pure = Sum
--   (<*>) = undefined
-- instance Monad Sum where
--   return = pure
--   (>>=) = undefined
--
-- instance (Num a) => Num (a, [b]) where
--   (+) a = bimap (fst a +) (snd a ++)
--   (*) = undefined
--   abs = undefined
--   signum = undefined
--   fromInteger = undefined
--   (-) = undefined
--
purchase :: String -> Integer -> Shopping
purchase item cost = Writer ((), Sum (cost, [item]))

total :: Shopping -> Integer
total (Writer (_, Sum (cost, _))) = cost

items :: Shopping -> [String]
items (Writer (_, Sum (_, item))) = item

-- shopping1 :: Shopping
-- shopping1 = do
--   purchase "Jeans" 19200
--   purchase "Water" 180
--   purchase "Lettuce" 328
--
-- items shopping1
-- (Sum 3, ["three"]) `mappend` (Sum 7, ["seven"])
--
newtype State s a =
  State
    { runState :: s -> (a, s)
    }

execState :: State s a -> s -> (a, s)
execState = runState

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f x = x <&> f

instance Applicative (State s) where
  pure :: a -> State s a
  pure s = State (s, )
  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) = undefined

instance Monad (State s) where
  return :: a -> State s a
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  m >>= k =
    State $ \s ->
      let (a, s) = runState m s
          m' = k a
       in runState m' s

readerToState :: Reader r a -> State r a
readerToState (Reader r) = State (\s -> (r s, s))

-- writerToState w = State (\s -> (fst (runWriter w), s `mappend` snd (runWriter w)))
writerToState :: Monoid w => Writer w a -> State w a
writerToState w = State (\s -> second (mappend s) (runWriter w))

-- runState (writerToState $ tell "world") "hello,"
--
fibStep :: State (Integer, Integer) ()
fibStep = State (\(s, a) -> ((), (a, a + s)))
--
-- execStateN :: Int -> State s a -> s -> s
-- execStateN n m = execState $ replicateM_ n m
-- fib :: Int -> Integer
-- fib n = fst $ execStateN n fibStep (0, 1)
--
-- fst $ execStateN 10 fibStep (0,1)
