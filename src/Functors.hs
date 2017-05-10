module Functors where

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a) deriving Show

instance Functor GeomPrimitive where
    fmap f (Point p)           = Point (fmap f p)
    fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)

data Tree' a = Leaf' a | Branch' (Tree' a) a (Tree' a)
    deriving Show

testTree :: Tree' Integer
testTree = Branch' (Leaf' 2) 3 (Leaf' 4)

instance Functor Tree' where
    fmap g (Leaf' x)       = Leaf' (g x)
    fmap g (Branch' l x r) = Branch' (fmap g l) (g x) (fmap g r)

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf m)       = Leaf (f <$> m)
    fmap f (Branch l m r) = Branch (f <$> l) (f <$> m) (f <$> r)

--instance Functor ((,) s) where
--    fmap g (x, y) = (x, g y)

data Entry k1 k2 v = Entry (k1, k2) v
                      deriving Show
newtype Map k1 k2 v = Map [Entry k1 k2 v]
                      deriving Show

instance Functor (Entry k1 k2) where
    fmap g (Entry keys v) = Entry keys (g v)

instance Functor (Map k1 k2) where
    fmap g (Map es) = Map $ map (fmap g) es

    {-Законы:
    fmap id      = id
    fmap (p . q) = (fmap p) . (fmap q)
    -}
