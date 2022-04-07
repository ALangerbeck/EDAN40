substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard t s = [replace wildcard x s | x <- t]

replace :: Eq a => a -> a -> [a] -> [a]
replace wildcard listElement listReplace
   | listElement == wildcard = listReplace
   | otherwise = [listElement]
