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
