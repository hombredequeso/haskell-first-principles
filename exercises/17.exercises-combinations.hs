module ExercisesCombinations where


import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

toWord c1 c2 c3 = (c1, c2, c3)

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 toWord

allWords = combos stops vowels stops
