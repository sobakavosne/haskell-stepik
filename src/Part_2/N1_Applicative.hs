{-# LANGUAGE InstanceSigs #-}

module Part_2.N1_Applicative where

import           Control.Applicative (ZipList (ZipList), getZipList)

newtype Arr2 e1 e2 a =
  Arr2
    { getArr2 :: e1 -> e2 -> a
    }

newtype Arr3 e1 e2 e3 a =
  Arr3
    { getArr3 :: e1 -> e2 -> e3 -> a
    }

instance Functor (Arr2 e1 e2) where
  fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
  fmap f (Arr2 e1e2ToA) = Arr2 (\e1 e2 -> f (e1e2ToA e1 e2))

instance Functor (Arr3 e1 e2 e3) where
  fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
  fmap f (Arr3 a) = Arr3 (\e1 e2 e3 -> f (a e1 e2 e3))

{-

Functor laws:

(1) fmap id container == container

(2) fmap f (fmap g container) == fmap (f . g) container

-}
{-

Proofs:

(1)

fmap id []                          -- def map
  == []

fmap id (x:xs)                      -- def map
  == (id x) : fmap id xs            -- def id
  == x : fmap id xs                 -- IH
  == x : xs

(2)

(base)

fmap (f . g) []                             -- def map
  == []

fmap f (fmap g [])                          -- def map
  == fmap f []                              -- def map
  == []

(induction step)

fmap (f . g) (x:xs)                         -- def map
  == ((f . g) x) : fmap (f . g) xs          -- def (.)
  == (f (g x)) : fmap (f . g) xs

fmap f (fmap g (x:xs))                      -- def map
  == fmap f ((g x) : fmap g xs)             -- def map
  == (f (g x)) : fmap f (fmap g xs)         -- def IH
  == (f (g x)) : fmap (\x1 -> f (g x1)) xs  -- def (.)
  == (f (g x)) : fmap (f . g) xs

-}
{-

Functor 🠒 Applicative Law

fmap g container ≡ pure g <*> container

-}
{-

Law for Pointed type classes - The free theorem:

fmap g (pure x) ≡ pure (g x)

-}
{-

Applicative laws:

Identity

pure id <*> v ≡ v

Homomorphism

pure g <*> pure x ≡ pure (g x)

Interchange

container <*> pure x ≡ pure ($ x) <*> container

-}
data Triple a =
  Tr a a a
  deriving (Eq, Show)

instance Functor Triple where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
  pure :: a -> Triple a
  pure a = Tr a a a
  (<*>) :: Triple (a -> b) -> Triple a -> Triple b
  (<*>) (Tr fa fb fc) (Tr a b c) = Tr (fa a) (fb b) (fc c)
-- Tr (^2) (+2) (*3) <*> Tr 2 3 4
--
-- $> pure id <*> [1,2,3]
--

x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]

(>$<) f xs = getZipList f 

(>*<) = zipWith

-- $> (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s