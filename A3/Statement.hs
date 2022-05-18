{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Statement(T, parse, toString, fromString, exec) where

{-
    Handed in by:
        Alfred Langerbeck al5878la-s@student.lu.se 
        Max Johansson, ma7580jo-s@student.lu.se
-}

import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import TestExpr (dict)
import Text.XHtml (variable, input, value)
import Distribution.Simple.Build (build)
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
    deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip :: Parser Statement
skip = accept "skip" # require ";" >-> buildSkip
buildSkip (_,_) = Skip

begin = accept "begin" -# iter parse #- require "end" >-> Begin

if' :: Parser Statement
if' = accept "if" -# Expr.parse #- require "then" # parse # require "else" -# parse >-> buildIf
buildIf ((e,state1),state2) = If e state1 state2

while :: Parser Statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (v,e) = While v e

read' :: Parser Statement
read' = accept "read" -# word #- require ";" >-> Read

write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> Write

comment :: Parser Statement
comment = accept "--" # iter (char ? (/='\n')) # require "\n" >-> buildComment
buildComment (string,_) = Comment (concat string)

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if Expr.value cond dict>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond whileStmts : stmts) dict input
    | Expr.value cond dict > 0 = exec (whileStmts : While cond whileStmts : stmts) dict input
    | otherwise = exec stmts dict input
exec (Assignment variable expression : stmts) dict input =  do
    let ndict = Dictionary.insert (variable, Expr.value expression dict) dict
    exec stmts ndict input
exec (Skip : stmts) dict input =
    exec stmts dict input
exec (Begin beginStmts : stmts) dict input =
    exec (beginStmts ++ stmts) dict input
exec (Write expression : stmts) dict input =
    Expr.value expression dict : exec stmts dict input
exec (Read variable :stmts) dict (input:inputs) = do
    let ndict = Dictionary.insert (variable, input) dict
    exec stmts ndict inputs
exec (Comment str : stmts) dict input =
    exec stmts dict input
exec [] _ _ = [] 

shw :: Int -> T -> String
shw prec (Assignment string expression) = indentString prec ++ string ++ " := " ++ toString expression ++";\n"
shw prec Skip  = indentString prec ++ "skip;\n"
shw prec (Begin statements) = indentString prec ++ "begin\n" ++ concat [ shw (prec+1) stat | stat <- statements] ++ "end\n"
shw prec (If expression statement1 statement2) = indentString prec ++ "if " ++ toString expression ++ " then\n" ++ shw (prec+1) statement1 ++ indentString prec ++ "else\n" ++ shw (prec+1) statement2 ++ "\n"
shw prec (While expression statement) = indentString prec ++ "while " ++ toString expression ++ " do\n" ++ shw (prec+1) statement
shw prec (Read string) = indentString prec ++ "read " ++ string ++ ";\n"
shw prec (Write expression) = indentString prec ++ "write " ++ toString expression ++ ";\n"
shw prec (Comment str) = indentString prec ++ "-- " ++ str ++ "\n"



indentString :: (Eq t, Num t) => t -> [Char]
indentString n =  case n of
        0 -> ""
        n -> "  " ++ indentString (n-1)

instance Parse Statement where
  parse = assignment ! skip ! begin ! if' ! while ! read' ! write ! comment
  toString = shw 0
