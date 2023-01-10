module Intro where

import Data.Char

sign 0 = 0
sign x | x > 0 = 1
       | otherwise = -1

-- -- $> sign (-12)

infix 5 |-|
(|-|) x y | n >= 0 = n 
          | otherwise = -n
            where n = x - y

-- -- $> 5 |-| 7

-- infixr 8         ^, `logBase`
-- infixl 7         *, /, `div`, `mod`
-- infixl 6         +, - 
-- infix 4          ==, >=, /=, >, <, <=

-- f $ x = f x      
-- infixr 0 $       right associativity with lowest priority

-- f (g x (h y)) == f $ g x $ h y

-- logBase 4 (min 20 (9 + 7))
-- logBase 4 $ min 20 $ 9 + 7

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y | isDigit x && isDigit y = read $ [x] ++ [y] :: Int
                  | otherwise              = 100

-- twoDigits2Int '1' '2'

-- the namespace of types and namespace of terms never intersect

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

-- dist (2,5) (7,12)

doubleFact :: Integer -> Integer
doubleFact n | n <= 0 = 1
             | otherwise = n * doubleFact (n - 2)

-- doubleFact 10

-- heavyweight Z Fibonacci
-- fibonacci :: Integer -> Integer
-- fibonacci n | isNegative = - (fibonacci' $ abs n)
--             | otherwise = fibonacci' $ abs n
--               where isNegative = n < 1 && even n
--                     fibonacci' 0 = 0
--                     fibonacci' 1 = 1
--                     fibonacci' x = fibonacci' (x - 1) + fibonacci' (x - 2)

-- Fibonacci sequence: a0​=0; a1​=1; ak=a(k−1)+a(k−2);

-- effective Z Fibonacci
fibonacci :: Int -> Int
fibonacci n | isNegative = - (fibonacci' $ abs n)
            | otherwise = fibonacci' $ abs n
              where isNegative = n < 1 && even n
                    fibonacci' 0 = 0
                    fibonacci' 1 = 1
                    fibonacci' x = fibs!!x
                      where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- fibonacci (-10)

-- specific recurrent sequence: a0​=1; a1​=2; a2​=3; ak​=a(k-1)​+a(k-2)​−2*a(k-3)​;
seqA :: Int -> Integer
seqA n = seqA'!!n
seqA' = 1 : 2 : 3 : zipWith (\y z -> z - 2 * y) seqA' (tail ys)  
  where ys = zipWith (+) (tail seqA') seqA'

-- seqA n = let
--   helper n a b c =
--     if n == 0 then a else helper (n - 1) b c (c + b - 2 * a)
--   in
--     helper n 1 2 3

-- seqA 301
-- 1276538859311178639666612897162414

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
  | x == 0 = (0,1)
  | otherwise = sum'n'count' $ abs x
    where sum'n'count' y = (sumDigits y, countDigits y) where {
      countDigits 0 = 0; 
      countDigits n = 1 + countDigits (div n 10); 
      sumDigits n = foldl (\acc y -> acc + read [y]) 0 (show n);
    }

-- sum'n'count (-10)

-- Trapezoidal Rule
-- h = (b - a) / n (step)
-- h/2 * (f x0 + 2 * f x1 + 2 * f x3 + ... + f x b)
-- where x0 = a; x1 = x0 + h; x2 = x1 + h; x3 = x2 + h

-- StackOverflow answer
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n
    | a <= b = - definiteIntegral b a g n
    | otherwise = δ * sum [ (g x + g (x+δ))/2
                          | x <- [a, a+δ .. b] ]
  where δ = (b-a) / fromIntegral n

-- definiteIntegral pi 0 sin 1000

--OpenAI ChatGPT answer
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b 
  | a == b    = 0
  | a > b     = - integration f b a
  | otherwise = go a b 0
  where
    h = (b - a) / 1000
    go x y sum
      | x >= b - h/2 = sum
      | otherwise    = go (x + h) y (sum + h * (f x + f (x + h)) / 2)

-- $> integration sin pi 0
