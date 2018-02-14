module Exercises12 where

-- example GHCi session
-- above the functions
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe "" = ""
replaceThe s = result where
    (nextWord, remainder) = splitAtNextWord s
    result = case notThe nextWord of
               Nothing -> "a" ++ (replaceThe remainder)
               Just w -> w ++ (replaceThe remainder)

splitAtNextWord :: String -> (String, String)
splitAtNextWord s = go ("", s) where
                        go tup@(_, "") = tup
                        go ("", ' ':cs) = (" ", cs)
                        go (w, r@(' ':cs)) = (w, r)
                        go (w, (c:cs)) = go (w ++ [c], cs)

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0
countTheBeforeVowel s = result where
    (nextWord, remainder) = splitAtNextWord s
    result = case (notThe nextWord) of
               Nothing -> 0 + (countTheBeforeVowel remainder)
               Just w -> case remainder of
                           "" -> 0
                           c:cs -> if (isVowel c) then (1 + countTheBeforeVowel cs) else (countTheBeforeVowel cs)

isVowel c = elem c "aeiou"
