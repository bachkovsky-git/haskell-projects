module Parser where

import           Control.Applicative ((*>), (<*))
import           Data.Char
import           Text.Parsec         hiding ((<|>))

-- test1 = parse letter "error text" "asd"
-- test2 = parse letter "error text" "1asd"

vowel :: Parsec String u Char
vowel = oneOf "aeiou"

-- parseTest getList "1;234;56"
getList :: Parsec String u [String]
getList = many1 digit `sepBy` char ';'

--parseTest p1 "aasasd123123" -> ("aasasd", "123123")
p0 :: Parsec String u (String, String)
p0 = (,) <$> many1 letter <*> many1 digit

--игнорирует пробелы перед цифрами
--parseTest p1 "aasasd             123123" -> ("aasasd", "123123")
p1 :: Parsec String u (String, String)
p1 = (,) <$> many1 letter <* many space <*> many1 digit
-- p1 = (,) <$> many1 letter <*> (many space *> many1 digit)

ignoreBraces :: Parsec String u a -> Parsec String u b
                                  -> Parsec String u c
                                  -> Parsec String u c
ignoreBraces lBr rBr inp = lBr *> inp <* rBr

newtype Parser a = Parser {apply :: String -> [(a, String)]}

parse' :: Parser a -> String -> a
parse' p = fst . head . apply p

anyChar' :: Parser Char
anyChar' = Parser f where
  f ""     = []
  f (c:cs) = [(c, cs)]

instance Functor Parser where
  f `fmap` p = Parser fun where
    fun s = [ (f a, s') | (a, s') <- apply p s]

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
  f `fmap` p = Prs fun where
    fun s = do
      (a, s') <- runPrs p s
      return (f a, s')

anyChr :: Prs Char
anyChr = Prs f where
  f ""     = Nothing
  f (c:cs) = Just (c, cs)


instance Applicative Parser where
  pure a = Parser fun where
    fun s = [(a, s)]

  pf <*> pv = Parser fun where
    fun s = [(g a, s'') | (g, s') <- apply pf s, (a, s'') <- apply pv s']

instance Applicative Prs where
  pure  a = Prs $ \s -> Just (a, s)

  pf <*> pv = Prs $ \s -> do
      (g, s')  <- runPrs pf s
      (a, s'') <- runPrs pv s'
      return (g a, s'')

satisfy' :: (Char -> Bool) -> Parser Char
satisfy' pr = Parser f where
  f ""                 = []
  f (c:cs) | pr c      = [(c, cs)]
           | otherwise = []

lower' :: Parser Char
lower' = satisfy' isLower

char' :: Char -> Parser Char
char' c = satisfy' (==c)

digit' :: Parser Int
digit' = digitToInt <$> satisfy' isDigit

multiplication' :: Parser Int
multiplication' = (*) <$> digit' <* char' '*' <*> digit'

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE pr = PrsE f where
  f ""                 = Left "unexpected end of input"
  f (c:cs) | pr c      = Right (c, cs)
           | otherwise = Left $ "unexpected " ++ [c]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

{-
instance Functor PrsE where
  f `fmap` p = PrsE $ \s -> case runPrsE p s of
    Left err      -> Left err
    Right (v, s') -> Right (f v, s')

instance Applicative PrsE where
  pure v    = PrsE $ \s -> Right(v, s)
  pf <*> pv = PrsE $ \s -> case runPrsE pf s of
    Left err      -> Left err
    Right (f, s') -> runPrsE (f <$> pv) s'
-}

instance Functor PrsE where
  f `fmap` p = PrsE $ \s -> do
    (v, s') <- runPrsE p s
    return (f v, s')

instance Applicative PrsE where
  pure v    = PrsE $ \s -> Right (v, s)
  pf <*> pv = PrsE $ \s -> do
    (f', s') <- runPrsE pf s
    (v, s'') <- runPrsE pv s'
    return (f' v, s'')

class Applicative f => Alternative f where
  empty :: f a                 -- эквивалент mempty
  (<|>) :: f a -> f a -> f a   -- эквивалент mappend

infixl 3 <|>

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  l       <|> _ = l
{--

right distributivity of <*>:
(f <|> g) <*> a === (f <*> a) <|> (g <*> a)

right absorption for <&>:
empty <*> a === empty

left distributivity of fmap
f <$> (a <|> b) === (f <$> a) <|> (f <$> b)

left absorption for fmap:
f <$> empty === empty
--}

instance Alternative Parser where
  empty = Parser $ const []

  p <|> q = Parser f where
    f s = let ps = apply p s
      in if null ps
        then apply q s
        else ps

instance Alternative Prs where
  empty = Prs $ const Nothing
  p <|> q = Prs $ \s -> runPrs p s <|> runPrs q s
  -- p <|> q = Prs $ \s -> case runPrs p s of
    -- Nothing -> runPrs q s
    -- res     -> res

lowers :: Parser String
lowers = pure (:) <*> lower' <*> lowers <|> pure ""

many' :: Alternative f => f a -> f [a]
many' p = (:) <$> p <*> many' p <|> pure []

satisfy'' :: (Char -> Bool) -> Prs Char
satisfy'' pr = Prs f where
  f ""                 = Nothing
  f (c:cs) | pr c      = Just (c, cs)
           | otherwise = Nothing

char'' :: Char -> Prs Char
char'' ch = satisfy'' (==ch)

many1' :: Prs a -> Prs [a]
many1' p = (:) <$> p <*> (many1' p <|> pure [])

mult :: Prs Int
mult = (*) <$> nat <* char'' '*' <*> nat

nat :: Prs Int
nat = Prs $ \s -> do
  (list, s') <- runPrs (many1' digit'') s
  return (foldl (\a b -> 10 * a + b) 0 list, s')
-- nat = Prs f where
--   f s = case runPrs (many1' digit'') s of
--     Just (list, s') -> Just (foldl (\a b -> 10*a + b) 0 list, s')
--     Nothing         -> Nothing

digit'' :: Prs Int
digit'' = digitToInt <$> satisfy'' isDigit
