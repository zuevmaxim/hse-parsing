{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Result ([a], b)) where
  show (Success ([], cs)) = "Empty tree"
  show (Success (ts, cs)) =  showList ts where
    showList [] = ""
    showList (c : cs) = (show c) ++ (showList cs)
  show (Error err) = "Syntax error: " ++ err

main :: IO ()
main = do
  runParser " 1 - 2 - 31;\nx = y"
  runParser "  1 * 2 - 3 / 4 + (-5) ^ 73;\nwidth = 14 + 128 * -length"
  runParser "[1 + 2, 3 + 4, [7, 8, 9]] ++ [a]; x = []; [1, 2, 3]"
  runParser "x = y ++ z"
  runParser ""
