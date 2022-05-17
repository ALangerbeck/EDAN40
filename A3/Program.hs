module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program prog) = concat [Statement.toString stm | stm <- prog]


{-
exec :: [Statement.T] -> [Integer]
exec statements = Statement.exec statements (Dictionary.empty) []
-}
exec :: T -> [Integer] -> [Integer]
exec (Program prog) = Statement.exec prog Dictionary.empty

--exec = error "Program.exec not implemented"