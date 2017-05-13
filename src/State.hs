module State where

import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

type User = String

type Password = String

type UsersTable = [(User, Password)]

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = reader $ \usrTable -> [user | (user, password) <- usrTable, password == "123456"]

--usersWithBadPasswords = do
--  users <- ask
--  return [user | (user, password) <- users, password == "123456"]
type Shopping = Writer (Sum Integer, [String]) ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans" 19200
  purchase "Water" 180
  purchase "Lettuce" 328

purchase :: String -> Integer -> Shopping
purchase item cost = tell (Sum cost, [item])

total :: Shopping -> Integer
total = getSum . fst . execWriter

items :: Shopping -> [String]
items = snd . execWriter

readerToState :: Reader r a -> State r a
readerToState m = state $ \s -> (runReader m s, s)

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = state $ \s -> (a, s `mappend` w) where (a, w) = runWriter m

fibStep :: State (Integer, Integer) ()
fibStep = state $ \ (x, y) -> ((), (y, x + y))

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM n m)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show)

numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (indepth tree) 1 where
  indepth (Leaf _) = do
    x <- get
    put (x + 1)
    return (Leaf x)
  indepth (Fork l _ r) = do
    ll <- indepth l
    x <- get
    put (x + 1)
    rr <- indepth r
    return (Fork ll x rr)
