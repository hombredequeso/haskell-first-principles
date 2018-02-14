module Cipher where
import Data.Char


lowerBase = ord 'a'
lowerHigh = ord 'z'
upperBase = ord 'A'
upperHigh = ord 'Z'
range = lowerHigh - lowerBase + 1

-- offset = 3

rescope base offset range i = (mod (i - base + offset) range) + base

caesarizeOrd::Int -> Int->Int
caesarizeOrd offset i
  | i >= lowerBase && i <= lowerHigh = rescope lowerBase offset range i
  | i >= upperBase && i <= upperHigh = rescope upperBase offset range i
  | otherwise = i

caesarize::Int -> Char -> Char
caesarize offset c = chr (caesarizeOrd offset (ord c))

caesar::Int -> String -> String
caesar offset cs = fmap (\x -> caesarize offset x) cs


unCaesar::Int->String->String
unCaesar offset cs = fmap (\x -> caesarize (negate offset) x) cs

--  vigenere cipher

type Keyword = String

vigenere:: Keyword -> String -> String
vigenere _ "" = ""
vigenere keyword text = encodedText where
    remainingText = drop (length keyword) text
    encodedText = (zipWith (\k c -> vigChar (vigOffset k) c) keyword text) ++ (vigenere keyword remainingText)

unvigenere:: Keyword -> String -> String
unvigenere _ "" = ""
unvigenere keyword text = decodedText where
    remainingText = drop (length keyword) text
    decodedText = (zipWith (\k c -> vigChar (negate (vigOffset k)) c) keyword text) ++ (unvigenere keyword remainingText)


vigOffset::Char -> Int
vigOffset c
  | c >= 'a' && c <= 'z' = (ord c) - (ord 'a') 
  | c >= 'A' && c <= 'Z' = (ord c) - (ord 'A') 
  | otherwise = 0


type CharToEncode = Char
vigChar:: Int -> CharToEncode -> Char
vigChar offset c = encodedChar where
                            i = ord c
                            encodedChar = chr (caesarizeOrd offset i)


