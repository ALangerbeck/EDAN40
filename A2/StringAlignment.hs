-- Hlint options
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import GHC.Base (maxInt)
import Data.Text.Array (equal)
--import System.Win32 (COORD(x))

{- Answers to Questions


-}

scoreMatch :: Int
scoreMatch = 0
scoreMismatch :: Int
scoreMismatch = -1
scoreSpace :: Int
scoreSpace = -1

string1 :: String
string1 = "writers"
string2 :: String
string2 = "vintner"

type AlignmentType = (String,String)


-- 2a
similarityScore :: String -> String -> Int
similarityScore _ [] = 0
similarityScore [] _ = 0
similarityScore string1 string2
 | (head string1 == '-') || (head string2 == '-') = scoreSpace + similarityScore (tail string1) (tail string2)
 | head string1 == head string2 = scoreMatch + similarityScore (tail string1) (tail string2)
 | otherwise = scoreMismatch + similarityScore (tail string1) (tail string2)


{-
2b: attachesHeads takes two elements and a list of tuples containing two lists
    it attaches h1 and h2 respectively as the head of the list in each tuple
    h1 to the first element in the tuple and h2 to the second element in the tuple
    it returns the list of new tuples with h1 and h2 added.
-}
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

--2b
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = do
    let maxValue = maximum [valueFcn x | x <- xs]
    filter (flip ((==) . valueFcn) maxValue) xs

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments (x:xs) (y:ys) = maximaBy applySimilarityScore (attachHeads x y (optAlignments xs ys) ++ attachHeads '-' y (optAlignments (x:xs) ys) ++ attachHeads x '-' (optAlignments xs (y:ys)))

-- Help functions
applySimilarityScore :: (String,String) -> Int
applySimilarityScore (string1,string2) = similarityScore string1 string2
