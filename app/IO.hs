module Main where

import           Control.Monad
import           Data.List        (isInfixOf)
import           System.Directory (getDirectoryContents, removeFile)

main :: IO ()
main = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  case name of
    "" -> main
    x  -> putStrLn $ "Hi, " ++ x ++ "!"

getLine' :: IO String
getLine' = do
  c <- getChar
  if c == '\n'
    then return ""
    else do
      cs <- getLine'
      return (c : cs)

putStr' :: String -> IO ()
putStr' = mapM_ putChar

-- putStr' = sequence_ . map putChar
putStrLn' :: String -> IO ()
putStrLn' s = sequence_ [putStr s, putChar '\n']

main' :: IO ()
main' = do
  putStr "Substring: "
  input <- getLine
  case input of
    "" -> putStrLn "Canceled"
    substr -> do
      files <- getDirectoryContents "."
      sequence_ [putStrLn ("Removing file: " ++ file) >> removeFile file | file <- files, substr `isInfixOf` file]
