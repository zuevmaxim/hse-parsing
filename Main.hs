module Main where

import Parser

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

main :: IO ()
main = do

  runParser " 1 - 2 - 3 + 4"
  runParser " maxval = 13 + 12"
  runParser " val = 3^5 + x^y"
  runParser " -3 ^ 5 + 2^cc"

