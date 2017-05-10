{-# LANGUAGE NoMonomorphismRestriction #-}
module Lists where
import Data.Char
import Data.List

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x1 x2 xs = x1 : x2 : xs

nTimes :: (Num t, Eq t) => t1 -> t -> [t1]
nTimes _ 0 = []
nTimes x n = x : nTimes x (n - 1)

head' :: [t] -> t
head' (x : _) = x
tail' :: [t] -> [t]
tail' (_ : xs) = xs

second :: [t] -> t
second (_ : x : _) = x
second1 :: [a] -> a
second1 (_ : xs) = head xs

oddsOnly :: Integral a => [a] -> [a]
oddsOnly = filter odd

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome (from : xs)  = (from == last xs) && isPalindrome (init xs)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 as bs cs = zipWith3 (\x y z -> x + y + z) (norm as) (norm bs) (norm cs)
    where
    maxLength = maximum [length as, length bs, length cs]
    norm xs = xs ++ replicate (maxLength - length xs) 0

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs = xgr : groupElems rest where (xgr, rest) = span (==head xs) xs

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter (\x -> p1 x || p2 x)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ltx ++ eqx ++ qsort gtx
    where
    ltx =  filter (<x) xs
    gtx = filter (>x) xs
    eqx = x : filter (==x) xs

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [x:ps | (hs, x:ts) <- (inits xs `zip` tails xs), ps <- perms (hs ++ ts)]
    where tails xs = map (\i -> drop i xs) [0..length xs]
          inits xs = map (\i -> take i xs) [0..length xs]

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isUpper) . words

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> maximum [x, y, z])

----- List generators ----------------------
ones :: [Integer]
ones = 1 : ones

nats :: Integer -> [Integer]
nats n = n : nats (n + 1)

repeat1 :: a -> [a]
repeat1 = iterate repeatHelper
repeatHelper :: a -> a
repeatHelper = id

filter' :: (t -> Bool) -> [t] -> [t]
filter' f xs = [x | x <- xs, f x]

----- List folding -----------
concatList :: [[a]] -> [a]
concatList = foldr (++) []

lengthList :: [a] -> Int
lengthList = foldr (\_ l -> l + 1) 0

sumOdd :: [Integer] -> Integer
sumOdd = foldr sumIfOdd 0 where
    sumIfOdd x s | odd x     = x + s
                 | otherwise = s

meanList :: [Double] -> Double
meanList xs = sum' / count where
    (sum', count) = foldr (\ x (s, l) -> (x + s, l + 1)) (0, 0) xs

evenOnly :: [a] -> [a]
evenOnly = map snd . filter (even . fst) . zip [1..]

lastElem :: [a] -> a
lastElem = foldl1 (flip const)

revRange :: (Char,Char) -> String
revRange = unfoldr g
  where g (fr, to) | to >= fr = Just (to, (fr, pred to))
                   | otherwise = Nothing
