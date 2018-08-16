
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IntegerParser where

import Control.Applicative
import Text.Trifecta
import Data.Foldable (asum)

import Test.Hspec

-- parseDigit :: Parser Char
-- parseDigit = char '0' <|> char '1'


digits :: [Char]
digits = "0123456789"

digitsP :: [Parser Char]
digitsP = char <$> digits

zeroParser :: Parser Char
zeroParser = char '0'

parseDigit :: Parser Char
parseDigit = asum digitsP
-- which is the same as:
-- parseDigit = foldr (<|>) zeroParser digitsP

toIntC :: Char -> Integer
toIntC c = toInteger ( ( fromEnum c ) - ( fromEnum '0' ) )

toInt :: [Char] -> Integer
toInt cs = foldr (\a b -> b*10 + (toIntC a) ) 0 cs


base10Integer :: Parser Integer
base10Integer = read <$> (some parseDigit)

instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True


main :: IO ()
main = hspec $ do

let testParseDigit =  parseString parseDigit mempty
let testParseInteger =  parseString base10Integer mempty

describe "parseDigit" $ do
    it "can parse 0" $ do
        let a = testParseDigit "0"
        
        a `shouldBe` Success ('0')

    it "can parse 1" $ do
        let a = testParseDigit "1"
        
        a `shouldBe` Success ('1')

    it "can parse all digits" $ do
        let a = parseString (some parseDigit) mempty "0123456789"
        a `shouldBe` Success ("0123456789")

describe "base10Integer" $ do
    it "can parse a simple integer" $ do
        let i = testParseInteger "12"
        i `shouldBe` Success(12)

