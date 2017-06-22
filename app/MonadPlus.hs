module MonadPlus where

import           Control.Applicative
import           Control.Monad

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Alternative PrsE where
  empty = PrsE f where
    f _ = Left "empty alternative"
  p <|> q = PrsE f where
    f s = let ps = runPrsE p s
      in if null ps
         then runPrsE q s
         else ps
{-

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  l       <|> _ = l

instance MonadPlus Maybe

-}


instance Functor PrsE where
  fmap = liftM

instance Applicative PrsE where
  pure  = return
  (<*>) = ap

instance Monad PrsE where
  return v    = PrsE $ \s -> Right (v, s)
  pa >>= f    = PrsE $ \s -> do
    (v, s') <- runPrsE pa s
    runPrsE (f v) s'

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
  f ""                 = Left "unexpected end of input"
  f (c:cs) | pr c      = Right (c, cs)
           | otherwise = Left $ "unexpected " ++ [c]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)


newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

charEP :: Char -> PrsEP Char
charEP c = satisfyEP (== c)

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP p = PrsEP ( g . succ ) where
  g pos ""     = (pos, Left $ "pos " ++ show pos ++ ": unexpected end of input")
  g pos (c:cs) | p c = (pos, Right (c, cs))
               | otherwise = (pos, Left $ "pos " ++ show pos ++ ": unexpected " ++ [c])

{-
satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
  f ""                 = Left "unexpected end of input"
  f (c:cs) | pr c      = Right (c, cs)
           | otherwise = Left $ "unexpected " ++ [c]
-}
