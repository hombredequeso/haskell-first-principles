{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module UsPhoneNumberParser where

import Control.Applicative
import Text.Trifecta
import Data.Foldable (asum)

import Test.Hspec


type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea
        Exchange LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = undefined

instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True

main :: IO ()
main = hspec $ do
    describe "parsePhone" $ do
        let testParsePhone = parseString parsePhone mempty

        it "Succeeds with variant 1" $ do
            (testParsePhone "123-456-7890") `shouldBe` 
                Success (PhoneNumber 123 456 7890)

        it "Succeeds with variant 2" $ do
            (testParsePhone "1234567890") `shouldBe` 
                Success (PhoneNumber 123 456 7890)

        it "Succeeds with variant 3" $ do
            (testParsePhone "(123) 456-7890") `shouldBe` 
                Success (PhoneNumber 123 456 7890)

        it "Succeeds with variant 4" $ do
            (testParsePhone "1-123-456-7890") `shouldBe` 
                Success (PhoneNumber 123 456 7890)

