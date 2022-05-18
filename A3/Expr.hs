{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Expr(Expr, T, parse, fromString, value, toString) where

{-
        Handed in by:
        Max Johansson, ma7580jo-s@student.lu.se
        Alfred Langerbeck, al5878la-s@student.lu.se
-}
{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail)
import Parser
    ( Parser,
      Parse(..),
      return,
      (!),
      (#),
      (>->),
      (#>),
      err,
      (#-),
      (-#),
      word,
      lit,
      number )
import qualified Dictionary

data Expr = Num Integer | Var String | Add Expr Expr
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr| Pow Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr, power :: Parser Expr

term', expr',factor' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

powOp :: Parser (Expr -> Expr -> Expr)
powOp = lit '^' >-> (\_ -> Pow)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = lit '*' >-> (\_ -> Mul) !
        lit '/' >-> (\_ -> Div)

addOp :: Parser (Expr -> Expr -> Expr)
addOp = lit '+' >-> (\_ -> Add) !
        lit '-' >-> (\_ -> Sub)

bldOp :: t1 -> (t1 -> t2 -> t3, t2) -> t3
bldOp e (oper,e') = oper e e'

power = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

factor' e = powOp # power >-> bldOp e #> factor' ! return e
factor = power #> factor'

term' e = mulOp # factor >-> bldOp e #> term' ! return e
term = factor #> term'

expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens :: Bool -> [Char] -> [Char]
parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
-- prec is importance of calculation
shw prec (Pow t u) = parens (prec>10) (shw 10 t ++ "^" ++ shw 10 u)

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num a) dict = a
value (Var a) dict = case Dictionary.lookup a dict of
        Nothing -> error ("Variable " ++ a ++ " is not in the dictionary, fool\n")
        Just x -> x
value (Add expr1 expr2) dict = value expr1 dict + value expr2 dict
value (Sub expr1 expr2) dict = value expr1 dict - value expr2 dict
value (Mul expr1 expr2) dict = value expr1 dict * value expr2 dict
value (Div expr1 expr2) dict = case value expr2 dict of
        0 -> error "You tried to divide by zero, fool\n"
        x -> div (value expr1 dict) (value expr2 dict)
value (Pow expr1 expr2) dict = value expr1 dict ^ value expr2 dict


instance Parse Expr where
    parse = expr
    toString = shw 0
