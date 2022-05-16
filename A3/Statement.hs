{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
import TestExpr (dict)
import Text.XHtml (variable, input, value)
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" # require ";" >-> buildSkip
buildSkip (_,_) = Skip

begin = accept "begin" -# iter parse #- require "end" >-> Begin

if' = accept "if" -# Expr.parse #- require "then" # parse # require "else" -# parse >-> buildIf
buildIf ((e,state1),state2) = If e state1 state2

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (v,e) = While v e

read' = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if Expr.value cond dict>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (While cond whileStmts : stmts) dict input
    | Expr.value cond dict > 0 = exec (While cond whileStmts : stmts) dict input
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
exec [] _ _ = []

shw :: Int -> T -> String
shw prec (Assignment string expression) = string ++ " = " ++ toString expression ++";\n"
shw prec Skip  = "skip;"
shw prec (Begin statements) = ""
shw prec (If expression statement1 statement2)= "if " ++ toString expression ++ " then\n" ++ shw 0 statement1 ++"else\n" ++ shw 0 statement2 ++ "\n" 
shw prec (While expression statement) = ""
shw prec (Read string) = "read " ++ string ++ ";\n"
shw prec (Write expression) = "write " ++ toString expression ++ ";\n"

instance Parse Statement where
  parse = assignment ! skip ! begin ! if' ! while ! read' ! write
  toString = shw 0
