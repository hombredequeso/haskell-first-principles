module AsExercises where

import Data.List (elemIndex, drop)
import Data.Char (toUpper)

-- First array is subseq of second array
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf (x:xs) l = result where
    pos = elemIndex x l
    result = go pos xs l

go:: (Eq a) =>  Maybe Int -> [a] -> [a] -> Bool
go Nothing _ _ = False
go (Just i) xs l = isSubseqOf xs (drop (i + 1) l)

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = fmap capitalize (words s) where

capitalize :: [Char] -> (String, String)
capitalize [] = ("", "")
capitalize w@(x:xs) = (w, (toUpper x) : xs)
