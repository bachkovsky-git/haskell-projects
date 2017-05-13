module Tree where

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show)

numberTree :: Tree () -> Tree Integer
numberTree tree = fst $ numberTree' tree 0

numberTree' :: Tree () -> Integer -> (Tree Integer, Integer)
numberTree' (Leaf _) n            = (Leaf n', n') where n' = n + 1
numberTree' (Fork left _ right) n = (Fork left' (n' + 1) right', n'') where
    (left', n') = numberTree' left n
    (right', n'') = numberTree' right (n' + 1)
