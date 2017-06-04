module Foldable'(Tree) where

import           Data.Monoid

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
  foldr f ini (Tr a b c) = foldr f ini [a, b, c]

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

testTree :: Tree Integer
testTree = Branch
              (Branch
                (Branch Nil 1 Nil)
                2
                (Branch Nil 3 Nil))
              4
              (Branch
                (Branch Nil 6 Nil)
                5
              (Branch Nil 7 Nil)
              )

instance Foldable Tree where
  foldr _ ini Nil            = ini
  foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Preorder where
  foldr _ ini (PreO Nil)            = ini
  foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))

instance Foldable Postorder where
  foldr _ ini (PostO Nil)            = ini
  foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)

instance Foldable Levelorder where
  foldr f ini (LevelO tr) = foldr f ini (tbf [tr])
    where
      tbf []    = []
      tbf [Nil] = []
      tbf xs    = map val xs ++ tbf (concatMap level xs)
      val (Branch _ a _) = a
      val Nil            = undefined
      level Nil                = []
      level (Branch Nil _ Nil) = []
      level (Branch Nil _ b)   = [b]
      level (Branch a _ Nil)   = [a]
      level (Branch a _ b)     = [a,b]

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo t = Endo $ foldr (.) id t
