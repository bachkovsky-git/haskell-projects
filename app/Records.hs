module Records where

import Data.Char (isDigit)
import qualified Data.Map.Lazy as M
import Data.Maybe
import qualified Data.Text as T
import Text.Read

--import Data.Time.Clock
--import Data.Time.Format
--import System.Locale
--timeToString :: UTCTime -> String
--timeToString = formatTime defaultTimeLocale "%a %d %T"
data LogLevel
  = Error
  | Warning
  | Info

data LogEntry = LogEntry
  { timestamp :: String
  , logLevel :: LogLevel
  , message :: String
  }

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString entry = timestamp entry ++ ": " ++ logLevelToString (logLevel entry) ++ ": " ++ message entry

data Coord a =
  Coord a
        a
  deriving (Show)

distance :: Coord Double -> Coord Double -> Double
distance (Coord x y) (Coord x' y') = sqrt ((x - x') ^ 2 + (y - y') ^ 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x y) (Coord x' y') = abs (x - x') + abs (y - y')

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord xn yn) = Coord (fromIntegral xn * w + w / 2) (fromIntegral yn * w + w / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x y) = Coord (floor (x / w)) (floor (y / w))

roots :: Double -> Double -> Double -> Either String (Double, Double)
roots a b c
  | discr >= 0 = Right (x1, x2)
  | otherwise = Left "Negative discriminant"
  where
    x1 = helper (-d)
    x2 = helper d
    helper x = (-b + x) / (2 * a)
    d = sqrt discr
    discr = b ^ 2 - 4 * a * c

findDigit :: String -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs)
  | isDigit x = Just x
  | otherwise = findDigit xs

findDigitOrX :: String -> Char
findDigitOrX str = fromMaybe 'X' (findDigit str)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

data Error
  = ParsingError
  | IncompleteDataError
  | IncorrectDataError String
  deriving (Show)

data Person = Person
  { firstName :: String
  , lastName :: String
  , age :: Int
  } deriving (Show)

parsePerson :: String -> Either Error Person
parsePerson str
  | length kvs < 3 = Left ParsingError
  | not (all valid pairs) = Left ParsingError
  | not (all present ["firstName", "lastName", "age"]) = Left IncompleteDataError
  | otherwise =
    case get "age" of
      Just ageTxt ->
        case (readMaybe ageStr :: Maybe Int) of
          Just ageInt ->
            case person of
              (Just p) -> Right p
              _ -> Left ParsingError
            where person = do
                    fn <- get "firstName"
                    ln <- get "lastName"
                    return (Person (T.unpack fn) (T.unpack ln) ageInt)
          Nothing -> Left $ IncorrectDataError ageStr
        where ageStr = T.unpack ageTxt
      Nothing -> Left IncompleteDataError
  where
    kvs = T.splitOn (T.pack "\n") (T.pack str)
    pairs = map (T.splitOn (T.pack " = ")) kvs
    valid p = length p == 2
    map' = M.fromList (map toTuple pairs)
      where
        toTuple [k, v] = (k, v)
    present = (`M.member` map') . T.pack
    get k = M.lookup (T.pack k) map'

--  | not validAge = Left
data Bit
  = Zero
  | One
  deriving (Eq, Show)

data Sign
  = Minus
  | Plus
  deriving (Eq, Show)

data Z =
  Z Sign
    [Bit]
  deriving (Eq, Show)

add :: Z -> Z -> Z
add z1 z2 = toZ (toDec z1 + toDec z2)

mul :: Z -> Z -> Z
mul z1 z2 = toZ (toDec z1 * toDec z2)

toDec :: Z -> Integer
toDec (Z s bs) =
  case s of
    Minus -> negate (toDec' bs)
    Plus -> toDec' bs
  where
    toDec' bs = sum (zipWith bitToInt bs [0 ..])
    bitToInt Zero = const 0
    bitToInt One = (2 ^)

toZ :: Integer -> Z
toZ i
  | i > 0 = Z Plus (bits i)
  | i == 0 = Z Plus []
  | otherwise = Z Minus (bits (abs i))
  where
    bits = reverse . map i2b . toBin
    i2b 1 = One
    i2b 0 = Zero
    toBin 0 = []
    toBin n
      | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
      | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]