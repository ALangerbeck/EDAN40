
{-
  Handed in by:
    Alfred Langerbeck al5878la-s@student.lu.se
    Max Johansson, ma7580jo-s@student.lu.se
-}
module Program(T, parse, fromString, toString, exec) where
import Parser ( Parse(..), (>->), iter )
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program prog) = concat [Statement.toString stm | stm <- prog]


exec :: T -> [Integer] -> [Integer]
exec (Program prog) = Statement.exec prog Dictionary.empty
