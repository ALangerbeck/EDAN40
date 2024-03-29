module Chatterbot where
import Utilities
import System.Random
import Data.Char
import qualified Data.Maybe
import System.Random

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]

--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind botbrain = do
  r <- randomIO :: IO Float
  return  $ rulesApply $ (map . map2) (id, pick r) botbrain

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply patterns phrase
  | Data.Maybe.isNothing (transformationsApply "*" reflect patterns phrase) = []
  | otherwise = Data.Maybe.fromJust (transformationsApply "*" reflect patterns phrase)

reflect :: Phrase -> Phrase
reflect [] = []
reflect (frstWord:wordTail)
  | lookup frstWord reflections /= Nothing = Data.Maybe.fromJust(lookup frstWord reflections): reflect wordTail
  | otherwise = frstWord:reflect wordTail

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile input  = map (map2 (words . map toLower,(map words))) input 

--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

-- first was only applied once but fixed by adding fix function, hope thats okay
reduce :: Phrase -> Phrase
reduce = fix $ reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply [] phrase = phrase 
reductionsApply (r:rs) phrase
  | match "*" (fst r) phrase /= Nothing = (substitute "*" (snd r))  (Data.Maybe.fromJust (match "*" (fst r) phrase))
  | otherwise = reductionsApply rs phrase



-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wc [] s = []
substitute wc (t:ts) s
  | wc == t = s ++ substitute wc ts s
  | otherwise = t : substitute wc ts s


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc (p:ps) (s:ss)
  | p == s = match wc ps ss
  | p == wc = orElse (longerWildcardMatch (p:ps) (s:ss)) (singleWildcardMatch (p:ps) (s:ss))
  | otherwise = Nothing


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
  |(match wc ps xs) /= Nothing =  Just [x]
  |otherwise = Nothing
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f trList (fstPattern, sndPattern)  = mmap (substitute wc sndPattern . f) (match wc fstPattern trList)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f (pattern:patternTail) trList = orElse (transformationApply wc f trList pattern) (transformationsApply wc f patternTail trList)
transformationsApply _ _ _ _ = Nothing
