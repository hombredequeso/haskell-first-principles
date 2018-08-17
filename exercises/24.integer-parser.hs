
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
--

toIntC :: Char -> Integer
toIntC c = toInteger ( ( fromEnum c ) - ( fromEnum '0' ) )

toInt :: [Char] -> Integer
toInt cs = foldl (\b a -> b*10 + (toIntC a) ) 0 cs

base10Integer :: Parser Integer
-- base10Integer = read <$> (some parseDigit)
base10Integer = toInt <$> (some parseDigit)

-- Another way...

parseDigitAsInt :: Parser Integer
parseDigitAsInt = toIntC <$> parseDigit 

parseDigitsToInts :: Parser [Integer]
parseDigitsToInts = some parseDigitAsInt


base10Integer' :: Parser Integer
base10Integer' = foldl (\b a -> b*10 + a) 0 parseDigitsToInts


instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True


main :: IO ()
main = hspec $ do

let testParseDigit =  parseString parseDigit mempty
let testParseInteger =  parseString base10Integer mempty

describe "toIntC" $ do
    it "can turn '1' into integer 1" $ do
        (toIntC '1') `shouldBe` 1

    it "can turn '0' into integer 0" $ do
        (toIntC '0') `shouldBe` 0
    it "can turn '9' into integer 9" $ do
        (toIntC '9') `shouldBe` 9

describe "toInt" $ do
    it "can turn \"12\" into Integer 12" $ do
        (toInt "12") `shouldBe` 12
    it "can turn \"2\" into Integer 2" $ do
        (toInt "2") `shouldBe` 2
    it "can turn \"012\" into Integer 12" $ do
        (toInt "012") `shouldBe` 12

describe "parseDigitAsInt" $ do
    it "can parse a digit char into an Int" $ do
        (parseString parseDigitAsInt mempty  "9") `shouldBe` Success(9)
    it "can parse a string with a digit as start into an Int" $ do
        (parseString parseDigitAsInt mempty   "9aa") `shouldBe` Success(9)
    it "can parse a 0 digit char into an Int 0" $ do
        (parseString parseDigitAsInt mempty  "0") `shouldBe` Success(0)

describe "parseDigitsToInts" $ do
    it "can parse digit string into an Ints" $ do
        (parseString parseDigitsToInts mempty  "123") `shouldBe` Success([1,2,3])


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

