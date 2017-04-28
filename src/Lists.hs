module Lists where
import Data.Char

addTwoElements x1 x2 xs = x1 : x2 : xs

nTimes x 0 = []
nTimes x n = x : nTimes x (n - 1)

head' (x : _) = x
tail' (_ : xs) = xs

second (_ : x : _) = x
second1 (_ : xs) = head xs


oddsOnly = filter odd

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome (fst : xs)  = (fst == last xs) && (isPalindrome $ init xs)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 as bs cs = zipWith3 (\x y z -> x + y + z) (norm as) (norm bs) (norm cs)
    where
    maxLength = maximum [length as, length bs, length cs]
    norm xs = xs ++ (replicate (maxLength - length xs) 0)

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
