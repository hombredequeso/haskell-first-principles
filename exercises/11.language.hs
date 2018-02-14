module Language where

import Data.Char (toUpper)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

getNextSentence:: String -> (String, String)
getNextSentence "" = ("", "")
getNextSentence (x: xs) = getNext ([x], xs) where

getNext::(String, String) -> (String, String)
getNext (s, "") = (s, "")
getNext (s, ('.':xs)) = (s++".", xs)
getNext (s, (x:xs)) = getNext(s++[x], xs)

capitalizeParagraph2 :: String -> String
capitalizeParagraph2 "" = ""
capitalizeParagraph2 s = (capitalizeWord next) ++ capitalizeParagraph2 theRest where
                            (next, theRest) = getNext ("", s)

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph ('.':xs) = "." ++ capitalizeParagraph xs
capitalizeParagraph (' ':xs) = " " ++ capitalizeParagraph xs
capitalizeParagraph p = result where
    (sentence, theRest) = break (\x -> x == '.') p
    result = capitalizeWord sentence ++ capitalizeParagraph theRest
    
