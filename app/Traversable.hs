{-# LANGUAGE TypeOperators #-}
module Traversable where

import           Data.Foldable        (asum, sequenceA_, traverse_)
import           Data.Functor.Compose
import           Data.Monoid
import           Data.Traversable

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

-- instance Foldable Tree where
--   foldr _ ini Nil            = ini
--   foldr f ini (Branch l x r) = f x (foldr f ( foldr f ini r) l) -- pre-order

-- instance Functor Tree where
--   fmap _ Nil            = Nil
--   fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

-- instance Traversable Tree where
--   traverse _ Nil            = pure Nil
--   traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r

-- testTree :: Tree Integer
-- testTree = Branch
--               (Branch
--                 (Branch Nil 1 Nil)
--                 2
--                 (Branch Nil 3 Nil))
--               4
--               (Branch
--                 (Branch Nil 6 Nil)
--                 5
--               (Branch Nil 7 Nil)
--               )

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f = foldr (\a fb -> (:) <$> f a <*> fb) (pure [])

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
  fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Foldable Triple where
  foldr f ini (Tr x y z) = foldr f ini [x, y, z]

instance Traversable Triple where
  traverse f (Tr x y z) = Tr <$> f x <*> f y <*> f z

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
  fmap _ (Error s) = Error s
  fmap f (Ok a)    = Ok (f a)

instance Foldable Result where
  foldr _ ini (Error _) = ini
  foldr f ini (Ok a)    = f a ini

instance Traversable Result where
  traverse _ (Error s) = pure (Error s)
  traverse f (Ok a)    = Ok <$> f a

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (f |.| g) where
  -- fmap :: (a -> b) -> (f |.| g) a -> (f |.| g) b
  h `fmap` (Cmps x) = Cmps $ fmap h <$> x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  -- pure :: a -> (f |.| g) a
  pure = Cmps . pure . pure
  Cmps h <*> Cmps x = Cmps $ (<*>) <$> h <*> x

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldr f ini (Cmps x) = foldr (flip (foldr f)) ini x

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
  traverse = undefined

xxx :: (Applicative f, Applicative g, Traversable t) => t (f (g a)) -> Compose f g (t a)
xxx = Compose . fmap sequenceA . sequenceA

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

instance Functor OddC where
  fmap f (Un x)     = Un (f x)
  fmap f (Bi x y z) = Bi (f x) (f y) (fmap f z)

instance Foldable OddC where
  foldMap f (Un x)     = f x
  foldMap f (Bi x y z) = f x <> f y <> foldMap f z

instance Traversable OddC where
  sequenceA (Un x)     = Un <$> x
  sequenceA (Bi x y z) = Bi <$> x <*> y <*> sequenceA z


-- data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Functor Tree where
  fmap = fmapDefault

instance Traversable Tree where
  traverse _ Nil            = pure Nil
  traverse f (Branch l x r) = (\ll rr xx -> Branch ll xx rr) <$> traverse f l <*> traverse f r <*> f x
