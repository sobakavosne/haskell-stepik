module Basics where

-- import Data.Function
-- left section
-- (1+)
-- right section
-- (+1)
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y

multSecond a b = on g h a b

g = (*)

h (_, y) = y

-- multSecond ('A',2) ('E',7)
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

sum3squares = (\x y z -> x + y + z) `on3` (^ 2)

-- sum3squares 1 2 3
class Printable a where
  toString :: a -> String
  toString _ = "unit type"

instance Printable Bool where
  toString True  = "true"
  toString False = "false"

instance Printable ()

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

-- toString (True,())
class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) =>
      KnownToGorkAndMork a
  where
  stompOrStab :: a -> a
  stompOrStab a
    | doesEnrageGork a && doesEnrageMork a = stomp $ stab a
    | doesEnrageMork a = stomp a
    | doesEnrageGork a = stab a
    | otherwise = a

ip = show a ++ show b ++ show c ++ show d

a = 127.22

b = 4.1

c = 20.1

d = 2

-- ip
class (Bounded a, Enum a, Eq a) =>
      SafeEnum a
  where
  ssucc :: a -> a
  ssucc a
    | a == maxBound = minBound
    | otherwise = succ a
  spred :: a -> a
  spred a
    | a == minBound = maxBound
    | otherwise = pred a

instance SafeEnum Bool

instance SafeEnum Int

-- spred (-9223372036854775808) :: Int
avg :: Int -> Int -> Int -> Double
avg a b c = (sum $ map fromIntegral [a, b, c]) / 3

-- avg 3 4 8
sumIt :: Int -> Int -> Int
sumIt x y = x + y
-- Reduction
{-
  Lazy strategy
  sumIt (1 + 2) 4
  -> (1 + 2) + 4
  -> 3 + 4
  -> 7

  Energetic strategy
  sumIt (1 + 2) 4
  -> sumIt 3 4
  -> 3 + 4
  -> 7
-}
{-
  id x = x
  const x y = x
  max x y = if x <= y then y else x
  infixr 0 $
  f $ x = f x

  const $ const (4 + 5) $ max 42
  const 9
-}
{-
  bar x y z = x + y
  foo a b = bar a a (a + b)
  value = foo (3 * 10) (5 - 2)

  value = bar (3 * 10) (3 * 10) ((3 * 10) + (5 - 2))
  value = (3 * 10) + (3 * 10)
  value = (30) + (3 * 10)
  value = (30) + (30)
  value = 60
-}

