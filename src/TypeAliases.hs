module TypeAliases where
--import Data.Map

type Endo a = a -> a

--type IntMap v = Data.Map.Map Int

newtype IntList = IList [Int] deriving Show
data IntList' = IList' [Int] deriving Show

ignore :: IntList -> String
ignore (IList _) = "Hello"

ignore' :: IntList' -> String
ignore' (IList' _) = "Hello"

{-
*TypeAliases> ignore undefined
"Hello"

*TypeAliases> ignore' undefined
"*** Exception: Prelude.undefined
-}

newtype A = A A
