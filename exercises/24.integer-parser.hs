
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

toIntCL :: [Char] -> [Integer]
toIntCL = fmap toIntC

toInt :: [Char] -> Integer
toInt cs = foldl (\b a -> b*10 + (toIntC a) ) 0 cs

base10Integer :: Parser Integer
-- base10Integer = read <$> (some parseDigit)
base10Integer = toInt <$> (some parseDigit)

negativeSignParser :: Parser Integer
negativeSignParser = (\_ -> -1) <$> char '-' 

negativeBase10IntegerParser :: Parser Integer
negativeBase10IntegerParser = negativeSignParser >> base10Integer' >>= return . negate

base10Integer' :: Parser Integer
base10Integer' = base10Integer <|> negativeBase10IntegerParser

-- Some unused compilable experiments (largely gone wrong, but interesting in their own right)!

firstDigitParser :: Parser Integer
firstDigitParser = negativeSignParser <|> ( toIntC <$>parseDigit )

digitsParser :: Parser [Char]
digitsParser = some parseDigit

digitsAsIntegerParser :: Parser [Integer]
digitsAsIntegerParser = toIntCL <$> digitsParser

negOrPosDigitsParser :: Parser [Integer]
negOrPosDigitsParser = (liftA2 (:)) firstDigitParser digitsAsIntegerParser

addNextDecimalPlace :: Integer -> Integer -> Integer
addNextDecimalPlace total next = (total * 10) + next

parseDigitAsInt :: Parser Integer
parseDigitAsInt = toIntC <$> parseDigit 

parseDigitsToInts :: Parser [Integer]
parseDigitsToInts = some parseDigitAsInt

instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True


main :: IO ()
main = hspec $ do

let testParseDigit =  parseString parseDigit mempty
let testParseInteger =  parseString base10Integer mempty


describe "addNextDecimalPlace" $ do
    it "adds new digit onto lowest decimal place of current integer" $ do
        (addNextDecimalPlace 0 1) `shouldBe` 1
        (addNextDecimalPlace 1 2) `shouldBe` 12


describe "firstDigitParser" $ do
    let testFirstDigitParser = parseString firstDigitParser mempty

    it "produces -1 when parsing negative sign" $ do
        (testFirstDigitParser "-") `shouldBe` Success(-1)

    it "produces integer digit when parsing a digit character" $ do
        (testFirstDigitParser "7") `shouldBe` Success(7)

describe "negativeSignParser" $ do

    let testNegativeSignParser =  parseString negativeSignParser mempty

    it "turns negative sign into -1" $ do
        (testNegativeSignParser "-") `shouldBe` Success(-1)


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


describe "negativeBase10IntegerParser" $ do

    let testNegativeBase10IntegerParser = parseString negativeBase10IntegerParser mempty

    it "can parse a negative integer" $ do
        let i = testNegativeBase10IntegerParser "-12"
        i `shouldBe` Success(-12)


describe "base10Integer'" $ do

    let testBase10Integer' =  parseString base10Integer' mempty
    it "can parse a simple integer" $ do
        let i = testBase10Integer' "12"
        i `shouldBe` Success(12)

    it "can parse a negative integer" $ do
        let i = testBase10Integer' "-12"
        i `shouldBe` Success(-12)
