{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module QCmessin where

-- Some helpful links:
--
    -- http://hackage.haskell.org/package/QuickCheck-2.12.4
    -- http://hackage.haskell.org/package/QuickCheck-2.12.4/docs/Test-QuickCheck.html
    -- http://hackage.haskell.org/package/QuickCheck-2.12.4/docs/Test-QuickCheck-Arbitrary.html
    -- http://hackage.haskell.org/package/QuickCheck-2.12.4/docs/Test-QuickCheck-Modifiers.html

import Data.Time
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Time

import Data.Time.LocalTime  -- TimeOfDay
import Data.Char
import Data.Text
import Data.Monoid

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

newtype MyPositive = 
    MyPositive Int
    deriving (Eq, Show)

toPositive :: Int -> Maybe MyPositive 
toPositive n = if (n > 0) then 
                          Just $ MyPositive n 
                          else 
                          Nothing

-- A bit silly: using Test.QuickCheck.Modifiers.Positive to implement MyPositive.
-- instance Arbitrary MyPositive where
--     arbitrary = do
--         (Positive a) <- arbitrary
--         return (MyPositive a) 

instance Bounded MyPositive where
    minBound = MyPositive 2
    maxBound = MyPositive 10

myPositiveGenerator :: Gen MyPositive
myPositiveGenerator = MyPositive <$> suchThat arbitrary (\x -> x > 0)

myLimitedPositiveGenerator :: Gen MyPositive
myLimitedPositiveGenerator = MyPositive <$> suchThat arbitrary (\x -> x > 10 && x < 100)

aroundNowDayGenerator :: Gen Day
aroundNowDayGenerator = ModifiedJulianDay <$> choose(50000, 60000)

instance Arbitrary MyPositive where
    arbitrary = myPositiveGenerator



toPositivePropertyIsNothingIfNotGreaterThanZero :: Int -> Bool
toPositivePropertyIsNothingIfNotGreaterThanZero x =
    if (x > 0) then
               toPositive x == ( Just $ MyPositive x )
               else
               toPositive x == Nothing

myPositiveIsAlwaysGreaterThanZero :: MyPositive -> Bool
myPositiveIsAlwaysGreaterThanZero (MyPositive n) =
    n > 0


-- ////////////////////////////////
--
--  Log entry:
--  Valid characters:
--      - upper/lower case ascii letters
--      - digits
--      - punctuation
--      - one '-' is valid
--      - not two '-' characters in a row.
--      - characters like '&'
--      - NOT a line separator
--  Length restrictions: 
--      - no longer than 80 characters
--      - at least one character
--
-- ////////////////////////////////

isValidLogCharacter :: Char -> Bool
isValidLogCharacter c = isAscii c && (isPrint c || c == '\t')

hasValidLogCharacters :: String -> Bool
hasValidLogCharacters = getAll . mconcat . (fmap ( All . isValidLogCharacter ) )

validLogLineRules :: [String -> Bool]
validLogLineRules =                
    [
        ( not . hasStartOfComment ),
        hasValidLogCharacters
    ] 

isValidLogLine :: String -> Bool
isValidLogLine s =  getAll $ mconcat 
                        (fmap 
                            (\rule -> (All . rule)s ) 
                            validLogLineRules)

hasStartOfComment :: String -> Bool
hasStartOfComment = (isInfixOf "--") . pack
main :: IO ()
main = 
    hspec $ do

        describe "isValidLogCharacter" $ do
            it "returns true for lower case ascii letters" $ do
                let genLCL = oneof $ fmap (pure.chr) [(ord 'a')..(ord 'z')]
                forAll genLCL isValidLogCharacter

            it "returns true for upper case ascii letters" $ do
                let genLCL = oneof $ fmap (pure.chr) [(ord 'A') .. (ord 'Z')]
                forAll genLCL isValidLogCharacter

            it "returns true for digits" $ do
                let genLCL = oneof $ fmap (pure.chr) [(ord '0') .. (ord '9')]
                forAll genLCL isValidLogCharacter

            it "does not include end-of-line characters" $ do
                let endOfLineCharacters = oneof $ fmap (pure) ['\n', '\r', '\f']
                forAll endOfLineCharacters $ not.isValidLogCharacter

            it "punctuation is valid" $ do
                let punctuationChars = oneof $ fmap (pure) ['-', '#', ' ', '&', '~', '@', '$', '%', '^', '&', '*', '(', ')']
                forAll punctuationChars $ isValidLogCharacter

            it "tab is valid" $ do
                isValidLogCharacter '\t'

        describe "hasStartOfComment" $ do

            it "is true if there are two dashes ('-') in a row" $ do
                hasStartOfComment "a --"

        describe "isValidLogLine" $ do

            it "is false if there are two dashes ('-') in a row" $ do
                not $ isValidLogLine "a --"

            it "is true for some sample log lines" $ do
                isValidLogLine "abcdef 123 $#@$!#!$#!   " `shouldBe` True
                isValidLogLine "            * 3   432  f3r2" `shouldBe` True
                
        describe "toPositive" $ do
            it "results in nothing if arg is not >0" $ do
                property toPositivePropertyIsNothingIfNotGreaterThanZero


        describe "MyPositive" $ do
            it "is always greater than 0" $ do
               property myPositiveIsAlwaysGreaterThanZero

            it "can use a specific Gen" $ do
                property  ( forAll myLimitedPositiveGenerator myPositiveIsAlwaysGreaterThanZero)

            it "can generate limited ranges using a specific Gen" $ do
                print "limited range start"
                sample myLimitedPositiveGenerator
                print "limited range end"


        describe "messin round" $ do
            it "can generate something random" $ do
                print "Start tests..."
                let f = arbitrary :: Gen Integer
                print "sample f:"
                sample f
                print "sample' f:"
                sample' f
                print "Finish tests."

    
        describe "quick-check-ing" $ do
            it "x + 1 is greater than x" $ do
                property $ \x -> x + 1 > (x::Int)

            it "could also be run outside of hspec, if we wanted" $ do
                 quickCheck prop_additionGreater

        describe "time generation" $ do
            it "can generate days" $ do
                sample (arbitrary :: Gen Day)

            it "can generate days around now" $ do
                sample aroundNowDayGenerator

            it "can generate some day around now" $ do
                dz <- generate aroundNowDayGenerator
                print dz

            it "can generate days 2" $ do
                sample (arbitrary :: Gen Day)

            it "can generate TimeOfDay" $ do
                sample (arbitrary :: Gen TimeOfDay)
