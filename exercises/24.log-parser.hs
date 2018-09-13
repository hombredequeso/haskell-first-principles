{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogParser where

import Control.Applicative
import Text.Trifecta
import Data.Time.LocalTime  -- TimeOfDay
import Data.Time.Clock      -- DiffTime
import Data.Time            -- Day
-- import Data.Foldable (asum)
import Text.RawString.QQ

import Test.Hspec
-- import Data.Char
import Data.String.Utils

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

data LogEvent =
    LogEvent TimeOfDay String
    deriving (Show, Eq)

data DateEvent =
    DateEvent Day
    deriving (Show, Eq)

data LoggingLine =
    LogLine LogEvent
    | LogDate DateEvent
    deriving (Show, Eq)


type Log = [LogEvent]
type Comment = String
type Activity = String

instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True

parseLog :: Parser [LoggingLine]
parseLog = do
    whiteSpace
    some parseLoggingLine

parseLoggingLine :: Parser LoggingLine
parseLoggingLine = (try (LogDate <$> parseDateLine))
                   <|> LogLine <$> parseEventLine

parseDateLine:: Parser DateEvent
parseDateLine = parseRemovingWhitespace (DateEvent <$> parseDate)

parseEventLine :: Parser LogEvent
parseEventLine = parseRemovingWhitespace parseLogEvent

parseRemovingWhitespace :: Parser t -> Parser t
parseRemovingWhitespace pt = do
    (many (parseComment >> whiteSpace))
    line <- pt
    whiteSpace
    return line

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

parseDate :: Parser Day
parseDate =
    char '#' >>
    many (char ' ') >>
    natural >>= \year ->
    char '-' >>
    natural >>= \month ->
    char '-' >>
    natural >>= \day ->
    return
        (fromGregorian 
            year 
            ( fromIntegral month ) 
            ( fromIntegral day ))


parseComment :: Parser Comment
parseComment = 
    string "--" >> 
    many (char ' ') >>
    many (noneOf "\n")

parseActivity :: Parser Activity
parseActivity = 
    try (manyTill (noneOf "\n") parseComment) <|>
        some (noneOf "\n")

parseLogEvent :: Parser LogEvent
parseLogEvent = do
    time <- parseTimeOfDay
    desc <- rstrip <$> parseActivity
    return $ LogEvent time desc

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")


-- parseLog :: Parser Log
-- parseLog = do
--     whiteSpace
--     some parseEventLine

durationBetweenEvents :: LogEvent -> LogEvent -> DiffTime
durationBetweenEvents (LogEvent t1 _) (LogEvent t2 _)= undefined

toLogWithDuration :: Log -> [(LogEvent, Maybe DiffTime)]
toLogWithDuration [] = []
toLogWithDuration (h : []) = [(h, Nothing)]
toLogWithDuration (h1 : h2 : t) = (h1, Just (durationBetweenEvents h1 h2)) : toLogWithDuration (h2 : t)

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

whiteSpaceAndCommentsThenOneEvent :: String
whiteSpaceAndCommentsThenOneEvent = [r|
    -- A comment

-- block of comments
-- block
-- block

-- Another comment
--

-- whatevs

08:00 Breakfast
|]

logDateLine :: String
logDateLine = [r|# 2018-07-24
|]

logDateAndEvent :: String
logDateAndEvent = [r|# 2018-07-24
08:00 Breakfast     
|]

logSample :: String
logSample = [r|
# 2018-07-24
08:00 Breakfast
09:00 Morning Tea
10:00 Sleep

# 2018-07-25
07:00 Breakfast
9:00 Work
11:30 Morning Tea
22:30 Sleep
|]

logSampleWithComments :: String
logSampleWithComments = [r|
-- This is a log...  
# 2018-07-24 -- this is day 1
08:00 Breakfast -- Yummy    
    09:00 Morning Tea
10:00 Sleep

-- Whatevs
--

# 2018-07-25      -- more commentary      
-- And even more    
    07:00 Breakfast
9:00 Work
11:30 Morning Tea
22:30 Sleep
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

    describe "parseLogEvent" $ do
        let testParseEvent = parseString parseLogEvent mempty

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
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogLine $ LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog twoLineLog) `shouldBe`
                Success expectedLog

        it "Parses a two line log file with no start of end Eols" $ do

            let expectedLog = 
                    [
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogLine $ LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog twoLineLogWithMinimumEols) `shouldBe`
                Success expectedLog

        it "Parses a two line log file with various odd whitespace" $ do

            let expectedLog = 
                    [
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogLine $ LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logWithSomeOddWhitespace) `shouldBe`
                Success expectedLog


        it "Parses a log file beginning with empty line" $ do
            let expectedLog = 
                    [
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogLine $ LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logStartingWithNewLine) `shouldBe`
                Success expectedLog


        it "Ignores a comment at the end of a log entry" $ do
            let expectedLog = 
                    [
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogLine $ LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog lineEndingWithComment) `shouldBe`
                Success expectedLog



        it "Parses a log file with comment line" $ do
            let expectedLog = 
                    [
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogLine $ LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logWithComment) `shouldBe`
                Success expectedLog


        it "Parses a log file with many comment lines" $ do
            let expectedLog = 
                    [
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogLine $ LogEvent (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logWithManyComments) `shouldBe`
                Success expectedLog


        it "Parses a log file with much whitespace and comments" $ do
            let expectedLog = 
                    [
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast"
                    ]
            (testParseLog whiteSpaceAndCommentsThenOneEvent) `shouldBe`
                Success expectedLog


    describe "parseDate" $ do
        let testParseDate = parseString parseDate mempty

        it "Parses a date correctly" $ do

            let expectedDate = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            (testParseDate "# 2018-07-24") `shouldBe`
                Success expectedDate

    describe "parseLoggingLine" $ do
        let testParseLoggingLine = parseString parseLoggingLine mempty

        it "Parses a date line correctly" $ do
            let expectedDate = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            let expectedResult = LogDate $ DateEvent expectedDate
            let result = testParseLoggingLine logDateLine
            result `shouldBe` Success expectedResult

        it "Parses a event line correctly" $ do
            let expectedResult = 
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast"
            let result = testParseLoggingLine oneLineLogWithComment
            result `shouldBe` Success expectedResult

    describe "parseLog" $ do
        let testParseLog = parseString parseLog mempty
        it "Parses a date line and event line correctly" $ do
            let expectedDate = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            let expectedResult = 
                    [
                        LogDate $ DateEvent expectedDate,
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast"
                    ]
            let result = testParseLog logDateAndEvent
            result `shouldBe` Success expectedResult


        it "Parses a complete sample log" $ do
            let expectedDate1 = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            let expectedDate2 = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 25 ))
            let expectedResult = 
                    [
                        LogDate $ DateEvent expectedDate1,
                        LogLine $ LogEvent (TimeOfDay 8 0 0) "Breakfast",
                        LogLine $ LogEvent (TimeOfDay 9 0 0) "Morning Tea",
                        LogLine $ LogEvent (TimeOfDay 10 0 0) "Sleep",
                        LogDate $ DateEvent expectedDate2,
                        LogLine $ LogEvent (TimeOfDay 7 0 0) "Breakfast",
                        LogLine $ LogEvent (TimeOfDay 9 0 0) "Work",
                        LogLine $ LogEvent (TimeOfDay 11 30 0) "Morning Tea",
                        LogLine $ LogEvent (TimeOfDay 22 30 0) "Sleep"
                    ]

            let result = testParseLog logSample
            result `shouldBe` Success expectedResult


            let resultFromCommentedLog = testParseLog logSampleWithComments
            resultFromCommentedLog `shouldBe` Success expectedResult
