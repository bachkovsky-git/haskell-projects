module Main where
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map        (Map, fromListWith)
import           Data.Ratio
import           Numeric
main :: a
main = undefined

fibonacci :: Int -> Integer
fibonacci n | n < 0       = (if n `mod` 2 == 0 then (-1) else 1) * fibonacci (-n)
            | otherwise  = fibs!!n

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

seqA :: Int -> Integer
seqA n = generate [1, 2, 3] n !! n

next :: [Integer] -> Integer
next xs = xs!!(l - 1) + xs!!(l - 2) - 2 * xs!!(l - 3) where l = length xs

generate :: [Integer] -> Int -> [Integer]
generate xs n | length xs > n = xs
              | otherwise     = xss ++ [next xss] where xss = generate xs (n - 1)

sumNCount :: Integer -> (Int, Int)
sumNCount x = (sum l, length l) where
  l = map digitToInt $ filter isDigit $ show x

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = delta * (f a + f b) / 2 + sum (map (f . seg) [2 .. n - 1])
    where
        n = 5000
        delta = (b - a) / n
        seg i = a + i * delta

getSecondFrom :: a -> b -> c -> b
getSecondFrom _ y _ = y

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

swap :: (b, a) -> (a, b)
swap =  uncurry (flip (,))
-- swap (1, "AS")

ip :: String
ip = show a ++ show b ++ show c ++ show d
a = 12
b = 7.22
c = 4.12
d = 0.12

avg :: Int -> Int -> Int -> Double
avg a b c = fromInteger (toInteger a + toInteger b + toInteger c) / 3.0

accum :: String -> String
accum s = intercalate "-"
  [ toUpper ch : replicate i (toLower ch) | (i, ch) <- zip [0..] s]

seriesSum :: Integer -> String
seriesSum n = showFFloat (Just 2) num "" where
  num = sum . map (1 / ) . genericTake n $ [1,4..]


anagrams :: String -> [String] -> [String]
anagrams w ws = [ word | word <- ws, anagram word == anagram w] where
  anagram s = fromListWith (+) $ zip s [1,1..]


solution :: String -> [String]
solution s | even (length s) = chunksOf 2 s
           | otherwise       = chunksOf 2 (s ++ "_")


findMissing :: Integral n => [n] -> n
findMissing xs@(f:s:t:_) = findM xs step where
  step = signum (s - f) * minimum (map abs [s - f, t - s])
  findM xs'@(f':s':_) n | f' + n == s' = findM (tail xs') n
                        | otherwise    = f' + n


x = findMissing (1:3:[7,9..])
