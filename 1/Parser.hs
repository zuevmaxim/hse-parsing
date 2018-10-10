module Parser where

import Tokenizer
import Prelude hiding (lookup)

data AST = ASum Operator AST AST
         | AProd Operator AST AST
         | AUnMinus Operator AST
         | APower Operator AST AST
         | AAssign String AST
         | ANum Integer
         | AIdent String

parse :: String -> Maybe AST
parse input =
  let ts = tokenize input in
  case ts of
    [TEof] -> Nothing
    _ -> let (tree, ts') = expression ts in
         if ts' == [TEof]
         then Just tree
         else error ("Parsing error on: " ++ show ts')

expression :: [Token] -> (AST, [Token])
expression ts =
  let (termNode, ts') = term ts in
  case lookup ts' of
    TOp op | op == Plus || op == Minus ->
      let (restExprNode, ts'') = restExpression termNode ts' in
      (restExprNode, ts'')
    TAssign ->
      case termNode of
        AIdent v -> let (exprNode, ts'') = expression $ accept ts' in
                    (AAssign v exprNode, ts'')
        _ -> error "Syntax error: assignment is only possible to identifiers"
    _ -> (termNode, ts')

restExpression :: AST -> [Token] -> (AST, [Token])
restExpression termNode ts =
  case lookup ts of
    TOp op | op == Plus || op == Minus ->
      let (termNode', ts') = term $ accept ts in
      let (restExprNode, ts'') = restExpression (ASum op termNode termNode') ts' in
      (restExprNode, ts'')
    _ -> (termNode, ts)

term :: [Token] -> (AST, [Token])
term ts =
  let (powNode, ts') = power ts in
  case lookup ts' of
    TOp op | op == Mult || op == Div ->
      let (restTermNode, ts'') = restTerm powNode ts' in
      (restTermNode, ts'')
    _ -> (powNode, ts')

restTerm :: AST -> [Token] -> (AST, [Token])
restTerm powNode ts =
  case lookup ts of
    TOp op | op == Mult || op == Div ->
      let (powNode', ts') = power $ accept ts in
      let (restTermNode, ts'') = restTerm (AProd op powNode powNode') ts' in
      (restTermNode, ts'')
    _ -> (powNode, ts)


power :: [Token] -> (AST, [Token])
power ts =
  let (factNode, ts') = factor ts in
  case lookup ts' of
    TOp op | op == Power ->
      let (powNode, ts'') = power $ accept ts' in
      (APower op factNode powNode, ts'')
    _ -> (factNode, ts')


factor :: [Token] -> (AST, [Token])
factor ts =
  case lookup ts of
    TLParen ->
      let (exprNode, ts') = expression $ accept ts in
      case lookup ts' of
        TRParen -> (exprNode, accept ts')
        _ -> error "Syntax error: mismatched parentheses"
    TIdent v -> (AIdent v, accept ts)
    TDigit d -> (ANum d, accept ts)
    TOp op | op == Minus ->
      let (a, ts') = power $ accept ts in
      (AUnMinus op a, ts')
    _ -> error "Syntax error: factor can only be a digit, an identifier, unary minus or a parenthesised expression"

lookup :: [Token] -> Token
lookup = head

accept :: [Token] -> [Token]
accept = tail

instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  APower op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AUnMinus op l -> showOp op : "\n" ++ show' (ident n) l
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  AIdent i     -> i)
      ident = (+1)
      showOp Plus  = '+'
      showOp Minus = '-'
      showOp Mult  = '*'
      showOp Div   = '/'
      showOp Power = '^'
