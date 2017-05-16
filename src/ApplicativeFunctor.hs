module ApplicativeFunctor where

class Functor f => Pointed f where
  pure' :: a -> f a

instance Pointed [] where
  pure' x = [x]

instance Pointed Maybe where
  pure' = Just

instance Monoid s => Pointed ((,) s) where
  pure' x = (mempty, x)

infixl 4 <**>
class Functor f => Apply f where
  (<**>) :: f (a -> b) -> f a -> f b

instance Apply [] where
  (<**>) = (<*>)

fmap2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
fmap2 g as bs = g <$> as <**> bs

fmap3 :: Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g as bs cs = g <$> as <**> bs <**> cs

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

{-
cont :: f a
g :: a -> b

fmap g cont === pure g <*> cont
-}

{-
1. Identity
pure id <*> v === v
2. Homomorphism
pure g <*> pure x === pure (g x)
3. Interchange
cnt <*> pure x === pure ($ x) <*> cont
4. Composition
pure (.) <*> u <*> v <*> cont === u <*> (v <*> cont)
-}
data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  op `fmap` (Tr x y z) = Tr (op x) (op y) (op z)

instance Applicative Triple where
  pure a = Tr a a a
  (Tr f g h) <*> (Tr x y z) = Tr (f x) (g y) (h z)

-- ZipList [(^2), (*3), (+100)] <*> Zip List [2, 1] = ZipList [4, 3]
newtype ZipList a = ZipList {getZipList :: [a]}
  deriving Show

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (fmap f xs)

instance Applicative ZipList where
  pure a = ZipList (repeat a)
  ZipList fs <*> ZipList es = ZipList $ zipWith ($) fs es

infixl 4 >$<
(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = (<$>)

infixl 4 >*<
(>*<) :: [a -> b] -> [a] -> [b]
zs >*< as = getZipList (ZipList zs <*> ZipList as)

x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]

t1 = (\a b -> 2*a+3*b) >$< x1s >*< x2s
t2 = (\a b c -> 2*a+3*b+5*c) >$< x1s >*< x2s >*< x3s
t3 = (\a b c d -> 2*a+3*b+5*c-4*d) >$< x1s >*< x2s >*< x3s >*< x4s
