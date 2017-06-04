{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PhantomTypes where
import           Control.Applicative

newtype Temperature a = Temperature Double
  deriving (Num, Eq, Show, Fractional)

data Celsius
data Fahrenheit
data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature k) = Temperature (k - 273.15)

test :: Bool
test = k2c 273.15 == Temperature 0

x :: Const Char b
x = Const 'z'
