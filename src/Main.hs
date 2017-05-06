module Main where
import Data.Char

main = undefined

fibonacci :: Int -> Integer
fibonacci n | n < 0       = (if n `mod` 2 == 0 then (-1) else 1) * fibonacci (-n)
            | otherwise  = fibs!!n

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


seqA :: Int -> Integer
seqA n = generate [1, 2, 3] n !! n

next :: [Integer] -> Integer
next xs = xs!!(l - 1) + xs!!(l - 2) - 2 * xs!!(l - 3) where l = length xs

generate :: [Integer] -> Int -> [Integer]
generate xs n | length xs > n = xs
              | otherwise     = xss ++ [next xss] where xss = generate xs (n - 1)

sum'n'count :: Integer -> (Int, Int)
sum'n'count x = (sum l, length l) where l = map digitToInt $ filter isDigit $ show x

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = delta * ((f a + f b) / 2 + (sum $ map f $ map seg $ [2..n - 1]))
    where
        n = 5000
        delta = (b - a) / n
        seg i = a + i * delta

getSecondFrom :: a -> b -> c -> b
getSecondFrom x y z = y

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

doItYourself :: Double -> Double
doItYourself = f . g . h

f = (logBase 2)
g = ( ^ 3)
h = (max 42)

swap =  uncurry (flip (,))
-- swap (1, "AS")

ip = show a ++ show b ++ show c ++ show d
a = 12
b = 7.22
c = 4.12
d = 0.12

avg :: Int -> Int -> Int -> Double
avg a b c = fromInteger (toInteger a + toInteger b + toInteger c) / 3.0