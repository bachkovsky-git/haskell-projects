module Monoids where


import Prelude hiding (lookup)
import qualified Data.List as L
import Data.Function

class Monoid' a where
    mempty' :: a             -- нейтральный элемент
    mappend' :: a -> a -> a  -- операция

    mconcat' :: [a] -> a     -- свертка
    mconcat' = foldr mappend' mempty'

{-законы:
[левый нейтральный элемент]  mempty `mappend` x === x
[правый нейтральный элемент] x `mappend` mempty === x
[ассоциативность]            (x `mappend` y) `mappend` z === x `mappend` ( y `mappend` z)
-}

instance Monoid' [a] where
    mempty' = []
    mappend' = (++)

newtype Sum' a = Sum' {getSum :: a}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Sum' a) where
    mempty' = Sum' 0
    Sum' x `mappend'` Sum' y = Sum' (x + y)

newtype Product' a = Product' {getProduct :: a}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Product' a) where
    mempty' = Product' 1
    Product' x `mappend'` Product' y = Product' (x * y)

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq, Show)

instance Monoid Xor where
    mempty = Xor False
    Xor x `mappend` Xor y = Xor $ x /= y

instance (Monoid' a, Monoid' b) => Monoid' (a, b) where
    mempty' = (mempty', mempty')
    (x1, x2) `mappend'` (y1, y2) = (x1 `mappend'` y1, x2 `mappend'` y2)

instance Monoid' a => Monoid' (Maybe a) where
    mempty' = Nothing
    Nothing `mappend'` a = a
    a `mappend'` Nothing = a
    Just a `mappend'` Just b = Just $ a `mappend'` b

newtype First' a = First' {getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

-- свертка всегда возвращает первый непустой элемент:

-- *Monoids> mconcat . map First' $  [Nothing, Nothing, Just 3, Just 5]
--   First' {getFirst = Just 3}

instance Monoid (First' a) where
    mempty = First' Nothing
    First' Nothing `mappend` r = r
    l `mappend` _ = l

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    mappend _ n@(Maybe' Nothing) = n
    mappend n@(Maybe' Nothing) _ = n
    Maybe' l `mappend` Maybe' r = Maybe' $ l `mappend` r


test0 = if (mempty :: Maybe' [Int]) == Maybe' Nothing then "failed" else "passed"

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap m) = L.lookup k m
    insert key val map = ListMap $ (key, val) : (getListMap $ delete key map)
    delete key (ListMap map) = ListMap $ filter (\ (k,_) -> k /= key) map

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty                       = ArrowMap $ \_ -> Nothing
    lookup x (ArrowMap f)       = f x
    insert key val (ArrowMap f) = ArrowMap $ \x -> if x == key then Just val else f x
    delete key (ArrowMap f)     = ArrowMap $ \x -> if x == key then Nothing else f x