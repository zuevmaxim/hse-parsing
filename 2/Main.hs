{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree
  show (Just (Error err)) = "Syntax error: " ++ err
  show Nothing = "Empty tree"

main :: IO ()
main = do
  runParser " 1 - 2 - 31;\nx = y"
  runParser "  1 * 2 - 3 / 4 + (-5) ^ 73;\nwidth = 14 + 128 * -length"
  runParser "[1 + 2, 3 + 4, [7, 8, 9]] ++ [a]; x = []; [1, 2, 3]"
  runParser "x = y ++ z"
  runParser ""
