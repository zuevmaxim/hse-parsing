module Tokenizer where

data Token = TDigit Integer
           | TIdent String
           | TOp Operator
           | TLParen
           | TRParen
           | TAssign
           | TEof
           deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Power
              deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEof]
tokenize (c : cs) | isOperator c   = TOp (operator c) : tokenize cs
                  | isDigit c      =
                    let (i, cs') = span isDigit cs in
                    let n = number (c : i) in
                    TDigit (n) : tokenize cs'
                  | isAlpha c      =
                    let (i, cs') = span isAlphaDigit cs in
                    let n = (c : i) in
                    TIdent (alpha n) : tokenize cs'
                  | c == '('       = TLParen : tokenize cs
                  | c == ')'       = TRParen : tokenize cs
                  | c == '='       = TAssign : tokenize cs
                  | isWhiteSpace c = tokenize cs
                  | otherwise = error ("Lexical error: unacceptable character " ++ [c])

isAlphaDigit :: Char -> Bool
isAlphaDigit x = or [isDigit x, isAlpha x]

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Power
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error("Wrong value!")

number :: String -> Integer
number x = fromJust(number' x)

number' :: String -> Maybe Integer
number' [] = Nothing
number' (c : cs) =
  let n = number' cs in
  case n of
    Nothing -> Just (digit c)
    _ -> Just ((digit c) * 10 + fromJust n)

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

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

alpha :: String -> String
alpha c = c

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"
