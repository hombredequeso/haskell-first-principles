module My96Ex where


import Data.List (intersperse, elemIndex)

equalsSeparator :: String -> Char -> Bool
equalsSeparator s = \x -> elemIndex x s /= Nothing

notSeparator :: String -> Char -> Bool
notSeparator s = \x -> elemIndex x s == Nothing

getWords :: String -> String -> [String]
getWords seps s = go s [] where
                isSeparator = equalsSeparator seps
                isNotSeparator = notSeparator seps
                charIsSepartor = \x -> elemIndex x seps
                go s a
                  | length s == 0 = reverse a
                  | (charIsSepartor (s !! 0)) /= Nothing = go (dropWhile isSeparator s) a
                  | otherwise = go (dropWhile isNotSeparator s) ((takeWhile isNotSeparator s) : a)

