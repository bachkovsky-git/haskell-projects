module Polymorphism where

class Printable a where
 toString :: a -> String

instance Printable Bool where
    toString True  = "true"
    toString False = "false"

instance Printable () where
    toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"


class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a
      | doesEnrageMork a && doesEnrageGork a = stomp $ stab a
      | doesEnrageMork a = stomp a
      | doesEnrageGork a = stab a
      | otherwise = a

-- cyclyc bounded enum
class (Eq a, Bounded a, Enum a) => SafeEnum a where
  ssucc, spred:: a -> a
  ssucc x = if x == maxBound then minBound else succ x
  spred x = if x == minBound then maxBound else pred x

instance SafeEnum Bool
