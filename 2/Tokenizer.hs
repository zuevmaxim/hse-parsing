
module Tokenizer where

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Power
              deriving (Show, Eq)

data ListOp = Concat

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/"

listOp :: String -> ListOp
listOp c | c == "++" = Concat
listOp c = error ("Lexical error: " ++ c ++ " is not an operator!")

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Power
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

digit :: Char -> Integer
digit c | c == '0' = 0
        | c == '1' = 1
        | c == '2' = 2
        | c == '3' = 3
        | c == '4' = 4
        | c == '5' = 5
        | c == '6' = 6
        | c == '7' = 7
        | c == '8' = 8
        | c == '9' = 9
digit c = error ("Lexical error: " ++ c : " is not a digit!")

number' :: [Char] -> (Integer, Integer)
number' (c : cs) | cs == [] = (digit c, 10)
                 | otherwise =
                   let (k, n) = number' cs in
                   (n * (digit c) + k, n * 10)
number' [] = error ("Not a number!")

number :: [Char] -> Integer
number cs =
  let (a, _) = number' cs in
  a

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

alpha :: String -> String
alpha c = c

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

isConcat :: String -> Bool
isConcat [c1, c2] = (c1 == '+' && c2 == '+')
isConcat _ = False
