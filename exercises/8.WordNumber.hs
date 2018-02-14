module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord = foldl (++) "" . intersperse "-" . map wordNumber . digits

digits :: Int -> [Int]
digits n = go n []
    where go n i 
                | n < 10 = (mod n 10):i
                | otherwise = go divResult (modResult:i)
                                where (divResult, modResult) = divMod n 10

nums = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
wordNumber :: Int -> String
wordNumber n = nums !! n

