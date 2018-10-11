module Parser (parse, AST) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

data AList

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | APower T.Operator AST AST
         | AUnMinus T.Operator AST
         | AAssign String AST
         | ANum Integer
         | AList [AST]
         | AIdent String
         | AConcat T.ListOp AST AST

parse :: Parser [AST]
parse =
  ( empty |> return [] )
  <|>
  ( expression >>= \e ->
    (
      ( empty |> return [e] )
      <|>
      ( sep |>
        ( ( empty |> zero "Input must not finish with ';'" )
          <|>
          ( parse >>= \es -> return (e : es) )
        )
      )
      <|> ( zero "syntax error" )
    )
  )

expression :: Parser AST
expression =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    expression >>= \e -> return (AAssign i e)
  )
  <|> ( identifier >>= \i ->
        concatOp >>= \op ->
        listExpression >>= \e -> return (AConcat op i e)
      )
  <|> ( countExpression >>= \e -> return e )
  <|> ( listExpression >>= \l -> return l)

countExpression :: Parser AST
countExpression =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    countExpression >>= \e -> return (AAssign i e)
  )
  <|> term >>= \l ->
  ( ( plusMinus  >>= \op -> countExpression >>= \r -> return (ASum op l r) )
    <|> return l
  )

listExpression :: Parser AST
listExpression =
  ( identifier >>= \(AIdent i) ->
        ( assignment |>
          listExpression >>= \l -> return (AAssign i l)
        )
        <|> ( concatOp >>= \op ->
          listExpression >>= \r -> return (AConcat op (AIdent i) r)
        )
        <|> return (AIdent i)
  )
  <|> ( list >>= \l ->
        ( concatOp >>= \op ->
          listExpression >>= \r -> return (AConcat op l r)
        )
        <|> return l
  )

list :: Parser AST
list =
  lSqparen |>
  (
    ( expression >>= \e ->
      listTail >>= \t ->
      rSqparen |> return (AList (e : t))
    )
    <|> ( rSqparen |> return (AList []) )
  )


listTail :: Parser [AST]
listTail =
  ( comma |>
    expression >>= \e ->
    listTail >>= \l -> return (e : l)
  )
  <|> return []

term :: Parser AST
term =
  -- make sure we don't reparse the factor
  power >>= \l ->
  ( ( divMult >>= \op ->
      term    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

power :: Parser AST
power =
  factor >>= \l ->
  ( ( pow >>= \op ->
      power >>= \r -> return (APower op l r)
    )
    <|> return l
  )

factor :: Parser AST
factor =
  ( lparen |>
    countExpression >>= \e ->
    rparen |> return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> number
  <|> ( unMinus >>= \op ->
        power >>= \r -> return (AUnMinus op r)
      )

number :: Parser AST
number     = map (ANum   . T.number) (span' T.isDigit)

identifier :: Parser AST
identifier = map (AIdent . T.alpha)  (span' T.isAlpha)

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

lSqparen :: Parser Char
lSqparen = char '['

rSqparen :: Parser Char
rSqparen = char ']'

assignment :: Parser Char
assignment = char '='

comma :: Parser Char
comma = char ','

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

unMinus :: Parser T.Operator
unMinus = map T.operator (char '-')

divMult :: Parser T.Operator
divMult = map T.operator (char '*' <|> char '/')

concatOp :: Parser T.ListOp
concatOp   = map T.listOp (sat T.isConcat elem2)

pow :: Parser T.Operator
pow = map T.operator (char '^')


instance Show AST where
  show tree = "\n" ++ show' 0 tree ++ "\n"
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  APower op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AUnMinus op r -> showOp op : "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AIdent i     -> i
                  AList (c : cs)     -> '[' : "\n" ++ show' (ident n) c ++ showListTail cs n
                  AList _ -> "[]"
                  AConcat op l r -> "++" ++ "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r)
      showListTail (c : cs) n = ',' : "\n" ++ show' (ident n) c ++ showListTail cs n
      showListTail []  n      = (if n >= 0 then "\n" ++ concat (replicate (n) "| ")  ++ "]" else "")
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'
      showOp T.Power = '^'
