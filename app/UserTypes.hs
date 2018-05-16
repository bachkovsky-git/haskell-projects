{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module UserTypes where
import           Data.Char

data MyBool = T | F deriving (Show, Eq, Read, Enum)
data MyType = S1 | Q1

alwaysTrue :: Int -> MyBool
alwaysTrue _ = T

not' :: MyBool -> MyBool
not' T = F
not' F = T

data Color = Red | Green | Blue

instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

data LogLevel = Error | Warning | Info -- тип сумма

cmp :: LogLevel -> LogLevel -> Ordering
cmp l1 l2 = compare (ord l1) (ord l2) where
    ord Error   = 2
    ord Warning = 1
    ord Info    = 0

data Result = Fail | Success
data SomeData = SD

doSomeWork :: SomeData -> (Result,Int)
doSomeWork = undefined

processData :: SomeData -> String
processData d = case doSomeWork d of
                    (_, 0) -> "Success"
                    (_, n) -> "Fail: " ++ show n


data Point = Point Double Double -- тип произведение

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x y) (Point x' y') = sqrt (abs (x' - x) ^ 2 + abs (y' - y) ^ 2)

data Roots = Roots Double Double | None -- тип сумма произведений
    deriving Show

roots :: Double -> Double -> Double -> Roots
roots a b c
    | discr >= 0 = Roots x1 x2
    | otherwise  = None
    where
        x1 = helper (-d)
        x2 = helper d
        helper x = (-b + x) / (2 * a)
        d = sqrt discr
        discr = b ^ 2 - 4 * a * c

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r)      = pi * r ^ 2
area (Rectangle w h) = w * h


data Result' = Success' | Fail' Int

instance Show Result' where
    show Success'  = "Success"
    show (Fail' n) = "Fail: " ++ show n

doSomeWork' :: SomeData -> Result'
doSomeWork' d = case doSomeWork d of
                    (Success, _) -> Success'
                    (Fail, n)    -> Fail' n

foo :: Bool -> Int
foo ~True = 1
foo False = 0

data Person = Person {firstName :: String, lastName :: String, age :: Int } deriving (Show, Eq)

abbrFirstName :: Person -> Person
abbrFirstName p@Person {firstName = f:_:_} = p {firstName = f : "." }
abbrFirstName p                            = p


data Coord a = Coord a a

data MapEntry a b where
  ME :: (Show a, Show b) => a -> b -> MapEntry a b

deriving instance Show (MapEntry a b)