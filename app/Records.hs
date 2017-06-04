module Records where
import           Data.Char  (isDigit)
import           Data.Maybe
--import Data.Time.Clock
--import Data.Time.Format
--import System.Locale

--timeToString :: UTCTime -> String
--timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { timestamp :: String, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString Error   = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info    = "Info"

logEntryToString :: LogEntry -> String
logEntryToString entry = timestamp entry ++
    ": " ++ logLevelToString (logLevel entry) ++ ": " ++ message entry


data Coord a = Coord a a deriving (Show)

distance :: Coord Double -> Coord Double -> Double
distance (Coord x y) (Coord x' y') = sqrt ((x - x') ^ 2 + (y - y') ^ 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x y) (Coord x' y') = abs (x - x') + abs (y - y')

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord xn yn) = Coord (fromIntegral xn * w + w / 2)  (fromIntegral yn * w + w / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x y) = Coord (floor (x/w)) (floor (y/w))

roots :: Double -> Double -> Double -> Either String (Double, Double)
roots a b c
    | discr >= 0 = Right (x1, x2)
    | otherwise  = Left "Negative discriminant"
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c


findDigit :: String -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) | isDigit x = Just x
                 | otherwise = findDigit xs

findDigitOrX :: String -> Char
findDigitOrX str = fromMaybe 'X' (findDigit str)

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

data Error = ParsingError | IncompleteDataError | IncorrectDataError String

data Person = Person { firstName :: String, lastName :: String, age :: Int }
