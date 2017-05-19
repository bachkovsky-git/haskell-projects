module Parser where

import           Control.Applicative ((*>), (<*))
import           Text.Parsec

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
  pure  a = Prs f where
    f s = Just (a, s)
  pf <*> pv = Prs f where
    f s = do
      (g, s') <- runPrs pf s
      (a, s'') <- runPrs pv s'
      return (g a, s'')
