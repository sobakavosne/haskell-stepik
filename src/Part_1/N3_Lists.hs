{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
module Part_1.N3_Lists where

import           Data.Char (isAlpha, isDigit, isUpper)
import           Data.List (find, foldl', unfoldr)
import           Prelude   hiding (reverse, (++))

nTimes :: a -> Int -> [a]
nTimes a n = times a n
  where
    times _ 0 = []
    times x y = x : times a (y - 1)

-- nTimes 'l' 10
--
second :: [a] -> a
second (_:x2:_) = x2

sndHead :: [(a, c)] -> c
sndHead = snd . head

-- sndHead [(1,2),(3,3),(5,10)]
--
(++) :: [a] -> [a] -> [a]
(++) [] ys     = ys
(++) (x:xs) ys = x : (++) xs ys

-- [1,2] ++ [2,1]
--
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x:xs)
  | odd x = x : oddsOnly xs
  | otherwise = oddsOnly xs

-- oddsOnly [1,2,3,4]
-- reverse' :: [a] -> [a]
-- reverse' [] = []
-- reverse' (x:xs) = (reverse' xs) ++ [x]
--
reverse :: [a] -> [a]
reverse l = rev l []
  where
    rev [] a     = a
    rev (x:xs) a = rev xs (x : a)

-- reverse [1,2,3]
--
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = fnc l []
  where
    fnc [] a     = a == l
    fnc (x:xs) a = fnc xs (x : a)

-- isPalindrome "saippuakivikauppias"
-- sum3 :: Num a => [a] -> [a] -> [a] -> [a]
-- sum3 [] [] []             = []
-- sum3 [] (y:ys) (z:zs)     = (y + z) : sum3 [] ys zs
-- sum3 (x:xs) [] (z:zs)     = (x + z) : sum3 xs [] zs
-- sum3 (x:xs) (y:ys) []     = (x + y) : sum3 xs ys []
-- sum3 (x:xs) [] []         = x : sum3 xs [] []
-- sum3 [] (y:ys) []         = y : sum3 [] ys []
-- sum3 [] [] (z:zs)         = z : sum3 [] [] zs
-- sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs
--
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 xs ys zs = xs `sum2` ys `sum2` zs
  where
    sum2 [] bs         = bs
    sum2 as []         = as
    sum2 (a:as) (b:bs) = (a + b) : sum2 as bs

-- sum3 [1,2,3] [1,2] [1,2,3,4]
--
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) =
  case groupElems xs of
    [] -> [[x]]
    (g:gs)
      | x == head g -> (x : g) : gs
      | otherwise -> [x] : g : gs

-- groupElems [1,2,2,5,5,5,1]
--
readDigits :: String -> (String, String)
-- readDigits x = (takeWhile isDigit x, dropWhile isDigit x)
readDigits = span isDigit

-- readDigits "365ads"
--
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj f1 f2 = filter (\x -> f1 x || f2 x)

-- filterDisj (< 10) odd [7,8,10,11,12]
--
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let smallerSorted = qsort [a | a <- xs, a <= x]
      biggerSorted = qsort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

-- squares'n'cubes [1,2,3]
--
perms :: [a] -> [[a]]
perms xs0 = xs0 : permutations xs0 []
  where
    permutations [] _ = []
    permutations (t:ts) is =
      foldr interleave (permutations ts (t : is)) (perms is)
      where
        interleave xs r =
          let (_, zs) = interleave' id xs r
           in zs
        interleave' _ [] r = (ts, r)
        interleave' f (y:ys) r =
          let (us, zs) = interleave' (f . (y :)) ys r
           in (y : us, f (t : y : us) : zs)

-- perms [1,2,3]
--
delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words

-- delAllUpper "Abc IS not ABC"
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> x `max` y `max` z)

-- max3 [1,2,3] [2,5,4] [1,2,1]
--
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- take 10 $ fibStream
--
newtype Odd =
  Odd Integer
  deriving (Eq, Show)

-- instance Enum Odd where
--   succ (Odd x) = Odd (x + 2)
--   pred (Odd x) = Odd (x - 2)
--   toEnum = Odd . toInteger
--   fromEnum (Odd x) = fromInteger x
--   enumFrom (Odd x) = Odd x : (enumFrom $ succ $ Odd x)
--   enumFromThen (Odd x) (Odd y)
--     | y == x = Odd x : enumFromThen (Odd x) (Odd y)
--     | otherwise = enumFromThen' (Odd x) (Odd y) (y - x)
--     where
--       enumFromThen' (Odd a) (Odd b) diff =
--         Odd a : enumFromThen' (Odd $ a + diff) (Odd b) diff
--   enumFromTo (Odd x) (Odd y)
--     | y > x = take (div (fromInteger $ y - x) 2 + 1) $ enumFrom (Odd x)
--     | otherwise = []
--   enumFromThenTo (Odd x1) (Odd x2) (Odd y)
--     | y > x2 && x2 > x1 =
--       take (div (abs (fromInteger (y - x1))) (fromInteger (x2 - x1)) + 1) $
--       enumFromThen (Odd x1) (Odd x2)
--     | y < x2 && x2 < x1 =
--       take (div (abs (fromInteger (y - x1))) (fromInteger (x1 - x2)) + 1) $
--       enumFromThen (Odd x1) (Odd x2)
--     | x2 == x1 && y >= x2 = Odd x1 : enumFromThenTo (Odd x1) (Odd x2) (Odd y)
--     | y == x1 = [Odd x1]
--     | otherwise = []
--
instance Enum Odd where
  toEnum i = Odd (toInteger i)
  fromEnum (Odd n) = fromEnum n
  succ (Odd n) = Odd (n + 2)
  pred (Odd n) = Odd (n - 2)
  enumFrom (Odd n) = map Odd [n,n + 2 ..]
  enumFromTo (Odd n) (Odd m) = map Odd [n,n + 2 .. m]
  enumFromThen (Odd n) (Odd n') = map Odd [n,n' ..]
  enumFromThenTo (Odd n) (Odd n') (Odd m) = map Odd [n,n' .. m]

-- Большое число, которое не поместится в Int
baseVal :: Integer
baseVal = 9900000000000000000

-- Генератор значений для тестирования
testVal n = Odd $ baseVal + n

-- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- testVal = id
test0 :: Bool
test0 = succ (testVal 1) == testVal 3

test1 :: Bool
test1 = pred (testVal 3) == testVal 1

-- enumFrom
test2 :: Bool
test2 = take 4 [testVal 1 ..] == [testVal 1, testVal 3, testVal 5, testVal 7]

-- enumFromTo
-- -- По возрастанию
test3 :: Bool
test3 =
  take 9 [testVal 1 .. testVal 7] ==
  [testVal 1, testVal 3, testVal 5, testVal 7]

-- -- По убыванию
test4 :: Bool
test4 = null (take 3 [testVal 7 .. testVal 1])

-- enumFromThen
-- -- По возрастанию
test5 :: Bool
test5 =
  take 4 [testVal 1,testVal 5 ..] ==
  [testVal 1, testVal 5, testVal 9, testVal 13]

-- -- По убыванию
test6 :: Bool
test6 =
  take 4 [testVal 5,testVal 3 ..] ==
  [testVal 5, testVal 3, testVal 1, testVal (-1)]

-- enumFromThenTo
-- -- По возрастанию
test7 :: Bool
test7 = [testVal 1,testVal 5 .. testVal 11] == [testVal 1, testVal 5, testVal 9]

-- -- По убыванию
test8 :: Bool
test8 =
  [testVal 7,testVal 5 .. testVal 1] ==
  [testVal 7, testVal 5, testVal 3, testVal 1]

-- -- x1 < x3 && x1 > x2
test9 :: Bool
test9 = null [testVal 7,testVal 5 .. testVal 11]

-- -- x1 > x3 && x1 < x2
test10 :: Bool
test10 = null [testVal 3,testVal 5 .. testVal 1]

test11 :: Bool
test11 = take 4 [testVal 5,testVal 5 ..] == replicate 4 (testVal 5)

test12 :: Bool
test12 = take 4 [testVal 5,testVal 5 .. testVal 11] == replicate 4 (testVal 5)

test13 :: Bool
test13 = take 4 [testVal 5,testVal 5 .. testVal 5] == replicate 4 (testVal 5)

test14 :: Bool
test14 = null [testVal 5,testVal 5 .. testVal 3]

test15 :: Bool
test15 = [testVal 5,testVal 1 .. testVal 5] == [testVal 5]

test16 :: Bool
test16 = toEnum (fromEnum (Odd 3)) == Odd 3

-- Это сомнительный тест. Скорее всего, его нет на stepik
test17 :: Bool
test17 = fromEnum (Odd 3) + 1 == fromEnum (Odd 5)

test18 :: Bool
test18 =
  [testVal 1,testVal 3 .. testVal 7] ==
  [testVal 1, testVal 3, testVal 5, testVal 7]

testList :: [Bool]
testList =
  [ test0
  , test1
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
  , test8
  , test9
  , test10
  , test11
  , test12
  , test13
  , test14
  , test15
  , test16
  , test17
  , test18
  ]

allTests :: [(Integer, Bool)]
allTests = zip [0 ..] testList

badTests :: [Integer]
badTests = map fst $ filter (not . snd) allTests

-- test0 = succ (Odd 1) == (Odd 3)
-- test1 = pred (Odd 3) == (Odd 1)
-- -- enumFrom
-- test2 = (take 3 $ [Odd 1 ..]) == [Odd 1,Odd 3,Odd 5]
-- -- enumFromTo
-- -- -- По возрастанию
-- test3 = (take 3 $ [Odd 1..Odd 7]) == [Odd 1,Odd 3,Odd 5]
-- -- -- По убыванию
-- test4 = (take 3 $ [Odd 7..Odd 1]) == []
-- -- enumFromThen
-- -- -- По возрастанию
-- test5 = (take 3 $ [Odd 1, Odd 3 ..]) == [Odd 1,Odd 3,Odd 5]
-- -- -- По убыванию
-- test6 = (take 3 $ [Odd 3, Odd 1 ..]) == [Odd 3,Odd 1,Odd (-1)]
-- -- enumFromThenTo
-- -- -- По возрастанию
-- test7 =([Odd 1, Odd 5 .. Odd 7]) == [Odd 1,Odd 5]
-- -- -- По убыванию
-- test8 =([Odd 7, Odd 5 .. Odd 1]) == [Odd 7,Odd 5,Odd 3,Odd 1]
-- -- -- x1 < x3 && x1 > x2
-- test9 =([Odd 7, Odd 5 .. Odd 11]) == []
-- -- -- x1 > x3 && x1 < x2
-- test10 =([Odd 3, Odd 5 .. Odd 1]) == []
-- allTests = zip [0..] [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]
-- Список тестов с ошибками
-- badTests
--
coins :: [Integer]
coins = [2, 3, 7]

-- change :: (Ord a, Num a) => a -> [[a]]
change :: Integer -> [[Integer]]
change s
  | s < minimum coins = []
  | otherwise = [xs | xs <- concat (subsetPermutations [coins]), sum xs == s]

subsetPermutations :: [[a]] -> [[[a]]]
subsetPermutations []     = []
subsetPermutations (y:ys) = [a : b | a <- y, b <- ys] : subsetPermutations ys

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f ini []     = ini
-- foldr f ini (x:xs) = x `f` foldr f ini
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f ini []     = []
-- foldl f ini (x:xs) = foldl f (f ini x) xs
--
lengthList :: [a] -> Int
lengthList = foldr (const succ) 0

-- subsetPermutations [coins]
sumOdd :: [Integer] -> Integer
sumOdd = sum . filter odd

-- sumOdd [1,2,3,4]
meanList :: [Double] -> Double
meanList xs = sum xs / foldr (const succ) 0 xs

-- meanList = uncurry (/) . foldr (\x (s,l) -> (x+s, l+1)) (0,0)
evenOnly :: [a] -> [a]
-- evenOnly :: forall a. [a] -> [a]
evenOnly =
  foldl'
    (\xs a ->
       if even (length xs)
         then xs ++ [a]
         else xs)
    []

--
--- $> take 3 (evenOnly [1..])
-- evenOnly =
--   fst .
--   foldl
--     (\(s, count) x ->
--        if even $ count + 1
--          then (s ++ [x], count + 1)
--          else (s, count + 1))
--     ([], 0)
-- evenOnly = snd . foldr (\a (xs, ys) -> (a : ys, xs)) ([], [])
-- в данном 🠑 решении список делится естественным образом на две части: в
-- процессе прохождения по списку пара (xs,ys) меняется местами и происходит
-- накопление ([odds], [evens])
--
lastElem :: [a] -> a
lastElem = foldl1 (const id)

-- find even [1,1,1]
--
revRange :: (Char, Char) -> [Char]
revRange = unfoldr g
  where
    g (b, a)
      | a < b = Nothing
      | a >= minBound && b <= maxBound = Just (a, (b, pred a))
      | otherwise = Nothing
--
-- $> revRange ('a', '\132')
