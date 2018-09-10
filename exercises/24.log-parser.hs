{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogParser where

import Control.Applicative
import Text.Trifecta
import Data.Time.LocalTime
-- import Data.Foldable (asum)
import Text.RawString.QQ

import Test.Hspec
-- import Data.Char
import Data.String.Utils

data LogEvent =
    LogEvent TimeOfDay String
    deriving (Show, Eq)

type Log = [LogEvent]
type Comment = String
type Activity = String

instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True

-- Notes on helpful combining functions:
--
-- some: one or more
-- many : zero or more
-- optional : one or none
--
-- class Applicative f => Alternative (f :: * -> *) where
--   some :: f a -> f [a]
--   many :: f a -> f [a]
--   optional :: Alternative f => f a -> f (Maybe a)

-- See esp:
-- https://hackage.haskell.org/package/parsers-0.12.9/docs/Text-Parser-Combinators.html



-- parseTimeOfDay, 'do' notation edition:
-- parseTimeOfDay :: Parser TimeOfDay
-- parseTimeOfDay = do
--     hour <- natural
--     _ <- char ':'
--     min <- natural
--     return 
--         (TimeOfDay 
--                 (fromIntegral hour)
--                 (fromIntegral min) 
--                 0)

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = 
    natural >>= \hour ->
    char ':' >>
    natural >>= \min ->
    return 
        (TimeOfDay 
                (fromIntegral hour)
                (fromIntegral min) 
                0)

parseComment :: Parser Comment
parseComment = do
    _ <- string "--"
    _ <- many (char ' ')
    comment <- many (noneOf "\n")
    return $ comment

parseActivity :: Parser Activity
parseActivity = 
    try (manyTill (noneOf "\n") parseComment) <|>
        some (noneOf "\n")

parseEvent :: Parser LogEvent
parseEvent = do
    time <- parseTimeOfDay
    desc <- rstrip <$> parseActivity
    return $ LogEvent time desc

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseEventLine :: Parser LogEvent
parseEventLine = do
    line <- parseEvent
    skipEOL
    whiteSpace
    return line

parseEventLineThrowAwayComments :: Parser LogEvent
parseEventLineThrowAwayComments = 
    (many (parseComment >> whiteSpace)) >> parseEventLine

parseLog :: Parser Log
parseLog = do
    -- Initially, throw away all whiteSpace
    whiteSpace
    some parseEventLineThrowAwayComments


-------------------------------------------------------------------
-- Test Logs
-------------------------------------------------------------------
    
oneLineLogWithComment :: String
oneLineLogWithComment = [r|08:00 Breakfast -- the comment
    |]

twoLineLog :: String
twoLineLog = [r|08:00 Breakfast
09:00 Morning Tea
    |]

twoLineLogWithMinimumEols :: String
twoLineLogWithMinimumEols = [r|08:00 Breakfast
09:00 Morning Tea|]

logStartingWithNewLine :: String
logStartingWithNewLine = [r|
08:00 Breakfast
09:00 Morning Tea
|]
  
logWithComment :: String
logWithComment = [r|
08:00 Breakfast
-- Whatevs, this is a comment and should be ignored
09:00 Morning Tea
|]


logWithManyComments :: String
logWithManyComments = [r|
08:00 Breakfast
-- Whatevs, this is a comment and should be ignored

--
-- Whatevs, this is a another and should be ignored
09:00 Morning Tea
|]

logWithSomeOddWhitespace :: String
logWithSomeOddWhitespace = [r|
08:00       Breakfast     
    09:00 Morning Tea       |]


logCommentLine :: String
logCommentLine = [r|--comment 123
|]

lineEndingWithComment :: String 
lineEndingWithComment = [r|08:00 Breakfast -- it was yummy
09:00 Morning Tea
|]



main :: IO ()
main = hspec $ do

    describe "parseComment" $ do
        let testParseComment = parseString parseComment mempty

        it "Parses a single line comment" $ do
            let expectedComment = "comment 123"
            (testParseComment logCommentLine) `shouldBe`
                Success expectedComment
        
    describe "parseTimeOfDay" $ do
        let testParseTimeOfDay = parseString parseTimeOfDay mempty

        it "can parse 08:00" $ do
            let expectedTimeOfDay = TimeOfDay 8 0 0
            (testParseTimeOfDay "08:00") `shouldBe`
                Success expectedTimeOfDay

        it "can parse 21:45" $ do
            let expectedTimeOfDay = TimeOfDay 21 45 0
            (testParseTimeOfDay "21:45") `shouldBe`
                Success expectedTimeOfDay

    describe "parseEvent" $ do
        let testParseEvent = parseString parseEvent mempty

        it "Parses a simple 'time description line'" $ do
            let expectedEvent = LogEvent (TimeOfDay 8 0 0) "Breakfast"
            (testParseEvent "08:00 Breakfast") `shouldBe` 
                Success expectedEvent

        it "ends where comment starts" $ do
            let expectedEvent = LogEvent (TimeOfDay 8 0 0) "Breakfast"
            (testParseEvent oneLineLogWithComment) `shouldBe` 
                Success expectedEvent

    describe "parseLog" $ do
        let testParseLog = parseString parseLog mempty

        it "Parses a two line log file" $ do

            let expectedLog = 
                    [
                        LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog twoLineLog) `shouldBe`
                Success expectedLog

        it "Parses a two line log file with no start of end Eols" $ do

            let expectedLog = 
                    [
                        LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog twoLineLogWithMinimumEols) `shouldBe`
                Success expectedLog

        it "Parses a two line log file with various odd whitespace" $ do

            let expectedLog = 
                    [
                        LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logWithSomeOddWhitespace) `shouldBe`
                Success expectedLog


        it "Parses a log file beginning with empty line" $ do
            let expectedLog = 
                    [
                        LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logStartingWithNewLine) `shouldBe`
                Success expectedLog


        it "Ignores a comment at the end of a log entry" $ do
            let expectedLog = 
                    [
                        LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog lineEndingWithComment) `shouldBe`
                Success expectedLog



        it "Parses a log file with comment line" $ do
            let expectedLog = 
                    [
                        LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logWithComment) `shouldBe`
                Success expectedLog


        it "Parses a log file with many comment lines" $ do
            let expectedLog = 
                    [
                        LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logWithManyComments) `shouldBe`
                Success expectedLog

