{-
    Authors: Alfred Langerbeck al5878la-s@student.lu.se
             Max Johansson ma7580jo-s@student.lu.se

    The hardest part of this assignment was the program optimization. This was because it was very abstract 
    and hard to understand compared to the rest of the assignment.

    The part we are most proud of was the optAlignment function. 
-}

-- Hlint options
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Hlint options
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-
    Authors: Alfred Langerbeck al5878la-s@student.lu.se
             Max Johansson ma7580jo-s@student.lu.se
-}

-- Hlint options
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Hlint options
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Text.Array (equal)
import Data.List (intersperse)


{- Answers to Questions


-}

scoreMatch :: Int
scoreMatch = 0
scoreMismatch :: Int
scoreMismatch = -1
scoreSpace :: Int
scoreSpace = -1

string1,string2,string3,string4,string5,string6 :: String
string1 = "writers"
string2 = "vintner"
string3 = "aferociousmonadatemyhamster"
string4 = "functionalprogrammingrules"
string5 = "bananrepubliksinvasionsarmestabsadjutant"
string6 = "kontrabasfiolfodralmakarmästarlärling"

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

--2c
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = do
    let maxValue = maximum [valueFcn x | x <- xs]
    filter (flip ((==) . valueFcn) maxValue) xs

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments (x:xs) (y:ys) = maximaBy applySimilarityScore (attachHeads x y (optAlignments xs ys) ++ attachHeads '-' y (optAlignments (x:xs) ys) ++ attachHeads x '-' (optAlignments xs (y:ys)))

-- 3 optimizing opt allignment
optOptAlignments :: String -> String -> [AlignmentType]
optOptAlignments string1 string2 = snd $ optAlign (length string1) (length string2)
    where
        optAlign i j = optTable!!i!!j
        optTable = [[ optEntry i j | j<-[0..]] | i<-[0..] ]

        rest :: Char -> Char -> (Int,[AlignmentType]) -> (Int,[AlignmentType])
        rest char1 char2 (restScore,restAlignments) = (similarityScore [char1] [char2] + restScore, attachTails char1 char2 restAlignments)

        optEntry :: Int -> Int -> (Int,[AlignmentType])
        optEntry 0 0 = (0,[([],[])])
        optEntry 0 j = rest '-' (string2!!(j-1)) $ optAlign 0 (j-1)
        optEntry i 0 = rest (string1!!(i-1)) '-'  $ optAlign (i-1) 0
        optEntry i j = (fst (head restsum),concatMap snd restsum)
            where
                restsum = maximaBy fst [rest (string1!!(i-1)) (string2!!(j-1)) $ optAlign (i-1) (j-1),rest (string1!!(i-1)) '-' $ optAlign (i-1) j, rest '-' (string2!!(j-1)) $ optAlign i (j-1)]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs ++ [h1],ys ++ [h2]) | (xs,ys) <- aList]

-- 3 optimize similarity score
similarityScore2 :: String -> String -> Int
similarityScore2 string1 string2 = score (length string1) (length string2)
    where
        score i j = scoreTable!!i!!j
        scoreTable = [[ scoreEntry i j | j<-[0..]] | i<-[0..] ]

        rest :: Char -> Char -> Int -> Int
        rest char1 char2 restScore = similarityScore [char1] [char2] + restScore

        scoreEntry :: Int -> Int -> Int
        scoreEntry 0 0 = 0
        scoreEntry i 0 = rest (string1!!(i-1)) '-' $ score (i-1) 0
        scoreEntry 0 j = rest '-' (string2!!(j-1)) $ score 0 (j-1)
        scoreEntry i j =  maximum [rest (string1!!(i-1)) (string2!!(j-1)) $ score (i-1) (j-1),
                                   rest (string1!!(i-1)) '-' $ score (i-1) j,
                                   rest '-' (string2!!(j-1))  $ score i (j-1)
                                  ]


outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
    let optlist = optOptAlignments string1 string2
        listLength = show (length optlist)
        similarity = similarityScore2 string1 string2
    putStrLn ("There are " ++ listLength ++ " optimal alignments: \n\n" ++ concatMap tupleOutput optlist ++ "There were " ++ listLength ++ " optimal alignments with similarity score: " ++ show similarity ++ "\n")


-- Help functions
applySimilarityScore :: AlignmentType -> Int
applySimilarityScore (string1,string2) = similarityScore string1 string2

tupleOutput :: AlignmentType -> String
tupleOutput (stringx,stringy) = intersperse ' ' stringx ++ "\n" ++ intersperse ' ' stringy ++ "\n\n"