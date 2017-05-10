module Monads where
import Control.Monad

data Log a = Log [String] a deriving (Show, Eq)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f s a = Log [s] (f a)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (m1 ++ m2) x'' where
    Log m1 x'  = f x
    Log m2 x'' = g x'

{-

class Monad m where                     -- m - однопараметрический тип (* -> *)
  return ::   a                 -> m a  -- упаковка значения в контейнер типа m

  (>>=)  :: m a -> (  a -> m b) -> m b  -- монадическое связывание (bind)

  (>>)   :: m a ->  m b         -> m b  -- облегченное связываие (отбрасывает первый аргумент, всегда возвращает второй,
                                        -- но сохраняет побочные эффекты, вызванные применением связывания к первому аргументу)
  fail   :: String -> m a               -- прерывание цепочки вычислений

-}

toKleisli :: Monad m => (a -> b) -> (a -> m b)
toKleisli f x = return (f x)
--toKleisli f = \x -> return (f x)

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log m1 x) f = Log (m1 ++ m2) x' where
    (Log m2 x') = f x

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return

newtype Identity a = Identity {runIdentity :: a}
    deriving (Show, Eq)

instance Functor Identity where
    fmap f x = x >>= return . f

instance Applicative Identity where
  pure = return
  Identity f <*> Identity v = Identity (f v)

instance Monad Identity where
    return = Identity
    Identity x >>= k = k x

wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ x = Identity (succ x)

{- первый закон
    return a >>= k === k a
   второй закон
    m >>= return === m
   третий закон
   m >>= k >>= k' === m >>= (\x -> k x >>= k')
-}
