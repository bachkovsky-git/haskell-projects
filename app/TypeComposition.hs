{-# LANGUAGE TypeOperators #-}
module TypeComposition where

infixr 9 |.|
newtype (|.|) f g a = Cmps {getCmps :: f (g a)}
  deriving (Eq, Show)

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
  foldr f ini (Cmps cmp) = foldr (flip (foldr f)) ini cmp
  foldMap f (Cmps x) = (foldMap . foldMap $ f) x

type A   = ((,) Integer |.| (,) Char) Bool

type B t = ((,,) Bool (t -> t) |.| Either String) Int

type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (1, ('c', True))

b :: B t
b = Cmps (True, id, Right 1)

c :: C
c = Cmps $ \p i -> if p then i else 0

instance (Functor f, Functor g) => Functor (f |.| g) where
  -- fmap :: (a -> b) -> (f |.| g) a -> (f |.| g) b
  h `fmap` (Cmps x) = Cmps $ fmap h `fmap` x

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) }
  deriving (Eq,Show)

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  f `fmap` (Cmps3 x) = Cmps3 $ (fmap . fmap) f `fmap` x

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
  -- pure :: a -> (f |.| g) a
  pure = Cmps . pure . pure
  (<*>) = undefined

-- GHCi> unCmps3 (pure 42 :: ([] |.| [] |.| []) Int)
-- [[[42]]]
unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 (Cmps x) = getCmps <$> x

-- GHCi> unCmps4 (pure 42 :: ([] |.| [] |.| [] |.| []) Int)
-- [[[[42]]]]
unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 (Cmps x) = unCmps3 <$> x
-- unCmps4 (Cmps x) = (getCmps <$>) . getCmps <$> x
