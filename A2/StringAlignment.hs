-- Hlint options
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import GHC.Base (maxInt)
import Data.Text.Array (equal)

{- Answers to Questions


-}


scoreMatch :: Int
scoreMatch = 1
scoreMismatch :: Int
scoreMismatch = -1
scoreSpace :: Int
scoreSpace = -2


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
    --let values = [valueFcn x | x <- xs]
    --let maxValue = maximum values
    --filter ((==) maxValue valueFcn) xs

maximaBy _ [] = []
maximaBy valueFcn xs
 | valueFcn (head xs) >= maximum (map valueFcn xs) = [head xs] ++ maximaBy valueFcn (tail xs)
 | otherwise = maximaBy valueFcn (tail xs)

-- maxValues <- filter maximum values-- maxIndexes <- elemIndex (maximum values) values

-- Help functions
maximum' :: Ord a => [a] -> a
maximum' [x]       = x
maximum' (x:x':xs) = maximum' ((if x >= x' then x else x'):xs)