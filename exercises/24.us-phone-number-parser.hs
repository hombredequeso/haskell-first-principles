{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UsPhoneNumberParser where

import Control.Applicative
import Text.Trifecta
import Data.Foldable (asum)
import Data.Sequence
import Data.Foldable (toList)

import Test.Hspec


type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea
        Exchange LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
    _ <- skipOptional prefixParser
    numberingPlanArea <- numberingPlanAreaParser <|> bracedNumberingPlanAreaParser
    _ <- skipMany separatorParser
    exchange <- exchangeParser
    _ <- skipMany separatorParser
    lineNumber <- lineNumberParser
    return (PhoneNumber numberingPlanArea exchange lineNumber)

-- or (simpler version of earlier parsePhone)
parsePhone' :: Parser PhoneNumber
parsePhone' = numberingPlanAreaParser >>= 
                \n -> exchangeParser >>=
                \e -> lineNumberParser >>=
                \l -> return $ PhoneNumber n e l


-----------------------------------------------------------------------------------

toInt :: Char -> Int
toInt c = fromIntegral $ toInteger ( ( fromEnum c ) - ( fromEnum '0' ) )

toInt' :: [Char] -> Int
toInt' = foldl (\acc c -> (acc * 10) + (toInt c)) 0

nDigitIntegerParser :: Int ->  Parser Int
nDigitIntegerParser n =  toInt' <$> (toList <$> (replicateA n digit) )

numberingPlanAreaParser :: Parser NumberingPlanArea
numberingPlanAreaParser =  nDigitIntegerParser 3

bracedNumberingPlanAreaParser :: Parser NumberingPlanArea
bracedNumberingPlanAreaParser = char '(' *> numberingPlanAreaParser <* char ')'

exchangeParser :: Parser Exchange
exchangeParser =  nDigitIntegerParser 3

lineNumberParser :: Parser LineNumber
lineNumberParser =  nDigitIntegerParser 4

separatorParser :: Parser Char
separatorParser = char ' ' <|> char '-'

prefixParser :: Parser String
prefixParser = string "1-"


-----------------------------------------------------------------------------------
    
instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True

main :: IO ()
main = hspec $ do

    describe "toInt'" $ do
        it "Parses a string into an integer" $ do
            (toInt' "123") `shouldBe` 123

    describe "numberingPlanAreaParser" $ do
        let testNumberingPlanAreaParser = parseString numberingPlanAreaParser mempty

        it "Succeeds with three digits" $ do
            (testNumberingPlanAreaParser "123") `shouldBe` 
                Success 123

    describe "exchangeParser" $ do
        let testExchangeParser = parseString exchangeParser mempty

        it "Succeeds with three digits" $ do
            (testExchangeParser "123") `shouldBe` 
                Success 123

    describe "lineNumberParser" $ do
        let testLineNumberParser = parseString lineNumberParser mempty

        it "Succeeds with four digits" $ do
            (testLineNumberParser "1234") `shouldBe` 
                Success 1234

    describe "parsePhone" $ do
        let testParsePhone = parseString parsePhone mempty

        it "Succeeds with variant 123-456-7890" $ do
            (testParsePhone "123-456-7890") `shouldBe` 
                Success (PhoneNumber 123 456 7890)

        it "Succeeds with variant 1234567890" $ do
            (testParsePhone "1234567890") `shouldBe` 
                Success (PhoneNumber 123 456 7890)

        it "Succeeds with variant (123) 456-7890" $ do
            (testParsePhone "(123) 456-7890") `shouldBe` 
                Success (PhoneNumber 123 456 7890)

        it "Succeeds with variant 1-123-456-7890" $ do
            (testParsePhone "1-123-456-7890") `shouldBe` 
                Success (PhoneNumber 123 456 7890)

