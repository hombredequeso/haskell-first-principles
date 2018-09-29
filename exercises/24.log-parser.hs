{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogParser where

import Control.Applicative
import Text.Trifecta
import Text.Printf
import Data.Time.LocalTime  -- TimeOfDay
import Data.Time.Clock      -- DiffTime
import Data.Time            -- Day
-- import Data.Foldable (asum)
import Text.RawString.QQ

import Test.Hspec
-- import Data.Char
import Data.String.Utils


import Test.QuickCheck  hiding (Failure, Success, Result)
import Test.QuickCheck.Instances.Time

import Data.Char
import Data.Text hiding (foldr)
import Data.Monoid
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

data EventLine =
    EventLine TimeOfDay String
    deriving (Eq)

instance Arbitrary EventLine where
    arbitrary = EventLine <$> arbitrary <*> logStringGenerator


data DateLine =
    DateLine Day
    deriving (Eq)

instance Arbitrary DateLine where
    arbitrary = DateLine <$> arbitrary

instance Arbitrary LogLine where
    arbitrary = frequency [
        (1, LogDateLine <$> arbitrary),
        (4, LogEventLine <$> arbitrary)]

data LogLine =
    LogEventLine EventLine
    | LogDateLine DateLine
    deriving (Eq)

formatLogTime :: TimeOfDay -> String
formatLogTime t = printf "%02d:%02d" (todHour t) (todMin t) 

instance Show EventLine where
    show (EventLine time desc) = ( formatLogTime time) ++ " " ++ desc



newtype LogLines = LogOf [LogLine]

instance Arbitrary LogLines where
    arbitrary = do
        len <- choose (20, 50)
        logLines <- vectorOf len (arbitrary :: Gen LogLine)
        return (LogOf logLines)

instance Show LogLines where
    show (LogOf ls) = foldr (\l acc -> (show l) ++ "\n" ++ acc) "" ls 


instance Show LogLine where
    show (LogEventLine l) = show l
    show (LogDateLine l) = show l

instance Show DateLine where
    show (DateLine day) = let (y,m,d) = toGregorian day in
        printf "# %04d-%02d-%02d" y m d

-- type Log = [EventLine]
type Comment = String
type Activity = String
type Event = (Day, TimeOfDay, String)
type TimedEvent = (Event, DiffTime)

newtype Log = Log [EventLine]

showLine :: EventLine -> String
showLine el = show el

instance Show Log where
    show (Log eventLines) = foldr (\l acc -> (showLine l) ++ acc) "" eventLines 


-- On dealing with time in Haskell see:
-- https://two-wrongs.com/haskell-time-library-tutorial
-- On realToFrac see:
-- https://stackoverflow.com/questions/25806378/how-do-i-convert-difftime-to-nominaldifftime

toTimedEvents :: [Event] -> [TimedEvent]
toTimedEvents (h1 : h2 : t) = (toTimedEvent h1 h2) : toTimedEvents (h2: t)
toTimedEvents _ = []

toTimedEvent :: Event -> Event -> TimedEvent
toTimedEvent (d1, t1, s1) (d2, t2, s2) = 
    -- let duration = secondsToDiffTime 60 in
                                    let duration = realToFrac ( diffUTCTime (UTCTime d2 ( timeOfDayToTime t2 )) (UTCTime d1 (timeOfDayToTime t1 ))) in
                                             ((d1,t1,s1), duration)

toEvents :: [LogLine] -> [Event]
toEvents = toEventsInner Nothing

toEventsInner :: Maybe Day -> [LogLine] -> [Event]
toEventsInner _ ((LogDateLine (DateLine day)):t)  = 
    toEventsInner (Just day) t
toEventsInner (Just day) ((LogEventLine (EventLine timeOfDay event)):t) = 
    (day, timeOfDay, event) : toEventsInner (Just day) t
toEventsInner _ _ = []

instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True

parseLog :: Parser [LogLine]
parseLog = do
    whiteSpace
    some parseLogLine

parseLogLine :: Parser LogLine
parseLogLine = (try (LogDateLine <$> parseDateLine))
                   <|> LogEventLine <$> parseEventLine

parseDateLine:: Parser DateLine
parseDateLine = parseRemovingWhitespace (DateLine <$> parseDay)

parseEventLine :: Parser EventLine
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

parseDay :: Parser Day
parseDay =
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

parseLogEvent :: Parser EventLine
parseLogEvent = do
    time <- parseTimeOfDay
    desc <- rstrip <$> parseActivity
    return $ EventLine time desc


-- parseLog :: Parser Log
-- parseLog = do
--     whiteSpace
--     some parseEventLine

durationBetweenEvents :: EventLine -> EventLine -> DiffTime
durationBetweenEvents (EventLine t1 _) (EventLine t2 _)= undefined

toLogWithDuration :: Log -> [(EventLine, Maybe DiffTime)]
toLogWithDuration (Log []) = []
toLogWithDuration (Log (h : [])) = [(h, Nothing)]
toLogWithDuration (Log (h1 : h2 : t)) = (h1, Just (durationBetweenEvents h1 h2)) : toLogWithDuration  ( Log (h2 : t) )


-------------------------------------------------------------------
    -- QuickCheck random log generation
-------------------------------------------------------------------

isValidLogCharacter :: Char -> Bool
isValidLogCharacter c = isAscii c && (isPrint c || c == '\t')

hasValidLogCharacters :: String -> Bool
hasValidLogCharacters = getAll . mconcat . (fmap ( All . isValidLogCharacter ) )

startsWithSpace :: String -> Bool
startsWithSpace "" = False
startsWithSpace (c:_) = isSpace c

endsWithSpace :: String -> Bool
endsWithSpace "" = False
endsWithSpace (c:[]) = isSpace c
endsWithSpace (c1:c2:xs) = endsWithSpace (c2:xs)

validLogLineRules :: [String -> Bool]
validLogLineRules =                
    [
        ( not . hasStartOfComment ),
        ( not . startsWithSpace ),
        ( not . endsWithSpace ),
        hasValidLogCharacters,
        -- greaterThanZeroLength,
        isNotAllWhitespace
    ] 

greaterThanZeroLength :: String -> Bool
greaterThanZeroLength s = 0 < Prelude.length s


isNotAllWhitespace :: String -> Bool
isNotAllWhitespace s = getAny $ mconcat ( fmap (Any . not . isSpace ) s )

isValidLogLine :: String -> Bool
isValidLogLine s =  getAll $ mconcat 
                        (fmap 
                            (\rule -> (All . rule)s ) 
                            validLogLineRules)

hasStartOfComment :: String -> Bool
hasStartOfComment = (isInfixOf "--") . pack

logStringGenerator :: Gen String
logStringGenerator = suchThat arbitrary isValidLogLine


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

logWithDate :: String
logWithDate = [r|# 2018-07-24
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
            let expectedEvent = EventLine (TimeOfDay 8 0 0) "Breakfast"
            (testParseEvent "08:00 Breakfast") `shouldBe` 
                Success expectedEvent

        it "ends where comment starts" $ do
            let expectedEvent = EventLine (TimeOfDay 8 0 0) "Breakfast"
            (testParseEvent oneLineLogWithComment) `shouldBe` 
                Success expectedEvent

    describe "parseLog" $ do
        let testParseLog = parseString parseLog mempty

        it "Parses a two line log file" $ do

            let expectedLog = 
                    [
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog twoLineLog) `shouldBe`
                Success expectedLog

        it "Parses a two line log file with no start of end Eols" $ do

            let expectedLog = 
                    [
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog twoLineLogWithMinimumEols) `shouldBe`
                Success expectedLog

        it "Parses a two line log file with various odd whitespace" $ do

            let expectedLog = 
                    [
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logWithSomeOddWhitespace) `shouldBe`
                Success expectedLog


        it "Parses a log file beginning with empty line" $ do
            let expectedLog = 
                    [
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logStartingWithNewLine) `shouldBe`
                Success expectedLog


        it "Ignores a comment at the end of a log entry" $ do
            let expectedLog = 
                    [
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog lineEndingWithComment) `shouldBe`
                Success expectedLog



        it "Parses a log file with comment line" $ do
            let expectedLog = 
                    [
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logWithComment) `shouldBe`
                Success expectedLog


        it "Parses a log file with many comment lines" $ do
            let expectedLog = 
                    [
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Morning Tea"
                    ]
            (testParseLog logWithManyComments) `shouldBe`
                Success expectedLog


        it "Parses a log file with much whitespace and comments" $ do
            let expectedLog = 
                    [
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast"
                    ]
            (testParseLog whiteSpaceAndCommentsThenOneEvent) `shouldBe`
                Success expectedLog


    describe "parseDay" $ do
        let testParseDate = parseString parseDay mempty

        it "Parses a date correctly" $ do

            let expectedDate = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            (testParseDate "# 2018-07-24") `shouldBe`
                Success expectedDate

    describe "parseLogLine" $ do
        let testParseLoggingLine = parseString parseLogLine mempty

        it "Parses a date line correctly" $ do
            let expectedDate = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            let expectedResult = LogDateLine $ DateLine expectedDate
            let result = testParseLoggingLine logWithDate
            result `shouldBe` Success expectedResult

        it "Parses a event line correctly" $ do
            let expectedResult = 
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast"
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
                        LogDateLine $ DateLine expectedDate,
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast"
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
                        LogDateLine $ DateLine expectedDate1,
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Morning Tea",
                        LogEventLine $ EventLine (TimeOfDay 10 0 0) "Sleep",
                        LogDateLine $ DateLine expectedDate2,
                        LogEventLine $ EventLine (TimeOfDay 7 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Work",
                        LogEventLine $ EventLine (TimeOfDay 11 30 0) "Morning Tea",
                        LogEventLine $ EventLine (TimeOfDay 22 30 0) "Sleep"
                    ]

            let result = testParseLog logSample
            result `shouldBe` Success expectedResult


            let resultFromCommentedLog = testParseLog logSampleWithComments
            resultFromCommentedLog `shouldBe` Success expectedResult



    let someDate = (fromGregorian 
            2018 
            ( fromIntegral 7 ) 
            ( fromIntegral 24 ))

    describe "toEvents" $ do
        it "returns empty for empty" $ do
            toEvents [] `shouldBe` []

        it "returns empty for date line only" $ do
            toEvents [
                        LogDateLine $ DateLine someDate
                     ]
                     `shouldBe` []

        it "returns empty for event line only" $ do
            toEvents [
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast"
                     ]
                     `shouldBe` []
        
        it "returns one event for date and event line only" $ do
            let someDate = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            toEvents [
                        LogDateLine $ DateLine someDate,
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast"
                     ]
                     `shouldBe` [(someDate, (TimeOfDay 8 0 0), "Breakfast")]
        
        it "returns one event for date and 2 event line only" $ do
            let someDate = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            toEvents [
                        LogDateLine $ DateLine someDate,
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 13 0 0) "Lunch"
                     ]
                     `shouldBe` [
                        (someDate, (TimeOfDay 8 0 0), "Breakfast"),
                        (someDate, (TimeOfDay 13 0 0), "Lunch")
                      ]
        
        it "Parses a complete sample log" $ do
            let expectedDate1 = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            let expectedDate2 = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 25 ))
            toEvents [
                        LogDateLine $ DateLine expectedDate1,
                        LogEventLine $ EventLine (TimeOfDay 8 0 0) "Breakfast",
                        LogEventLine $ EventLine (TimeOfDay 9 0 0) "Morning Tea",
                        LogDateLine $ DateLine expectedDate2,
                        LogEventLine $ EventLine (TimeOfDay 7 0 0) "Breakfast2"
                    ]
                    `shouldBe` [
                        (expectedDate1, (TimeOfDay 8 0 0), "Breakfast"),
                        (expectedDate1, (TimeOfDay 9 0 0), "Morning Tea"),
                        (expectedDate2, (TimeOfDay 7 0 0), "Breakfast2")
                               ]


    describe "toTimedEvents" $ do
        it "returns empty for empty" $ do
            toTimedEvents [] `shouldBe` []

        it "returns empty for a single event" $ do
            let expectedDate1 = (fromGregorian 
                    2018 
                    ( fromIntegral 7 ) 
                    ( fromIntegral 24 ))
            toTimedEvents   [
                            (expectedDate1, (TimeOfDay 8 0 0), "Breakfast")
                            ]
                `shouldBe` []

        it "returns expected event and time for 2 events in log" $ do
            let expectedDate1 = fromGregorian 
                                    2018 
                                    ( fromIntegral 7 ) 
                                    ( fromIntegral 24 )
            let duration = (secondsToDiffTime 60 * 60)
            toTimedEvents   
                [
                    (expectedDate1, (TimeOfDay 8 0 0), "Breakfast"),
                    (expectedDate1, (TimeOfDay 9 0 0), "Morning Tea")
                ]
                `shouldBe` 
                    [
                        ((expectedDate1, (TimeOfDay 8 0 0), "Breakfast"), duration)
                    ]

    describe "show EventLine produces a valid log event line" $ do
        it "returns a valid log line" $ do
            let timeOfDay = (TimeOfDay 8 0 0)
            let eventLine = EventLine timeOfDay "event description"
            show eventLine `shouldBe` "08:00 event description"

    describe "formatLogTime" $ do
        it "return valid log time for 1:2:3" $ do
            formatLogTime (TimeOfDay 1 2 3) `shouldBe` "01:02"

        it "return valid log time for 14:45:55" $ do
            formatLogTime (TimeOfDay 14 45 55) `shouldBe` "14:45"

    describe "Show DateLine" $ do
        it "returns a string formatted in logging style" $ do
            let date = fromGregorian 
                                    2018 
                                    ( fromIntegral 7 ) 
                                    ( fromIntegral 24 )
            show (DateLine date) `shouldBe` "# 2018-07-24"

    let getLogLine = 
            let timeOfDay = (TimeOfDay 8 0 0) in
            let eventLine = EventLine timeOfDay "event description" in
            LogEventLine eventLine 

    let getLogDateLine =
            let date = fromGregorian 
                                    2018 
                                    ( fromIntegral 7 ) 
                                    ( fromIntegral 24 ) in
            LogDateLine $ DateLine date


    describe "Show LogLine" $ do
        it "shows a LogEventLine correctly" $ do
            let logLine = getLogLine
            show logLine `shouldBe` "08:00 event description"

        it "shows a LogDateLine correctly" $ do
            let date = fromGregorian 
                                    2018 
                                    ( fromIntegral 7 ) 
                                    ( fromIntegral 24 )
            show (LogDateLine $ DateLine date) `shouldBe` "# 2018-07-24"

    let expectedLogLines = [r|# 2018-07-24
08:00 event description
|]

    describe "Show LogLines" $ do
        it "produces a valid log" $ do
            let logLines = LogOf [getLogDateLine, getLogLine]
            show logLines `shouldBe` expectedLogLines

    -- Quickcheck tests.
    
    describe "QuickCheck log line production" $ do

        let testParseLog = parseString parseLog mempty

        it "can generate a random log date line" $ do
            d <- generate (arbitrary :: Gen Day)
            let ldl = LogDateLine $ DateLine d
            print ldl
            
        it "can generate a random log event line" $ do
            s <- generate logStringGenerator
            print s

        it "can generate a LogLine" $ do
            sample (arbitrary :: Gen LogLine)

        it "can randomly generate log lines and show them" $ do
            lls <- generate (arbitrary :: Gen LogLines)
            print lls

        -- it "log lines round trip" $ do
        --     lls <- generate (arbitrary :: Gen LogLines)     -- LogLines
        --     let logStr = show lls
        --     let parsedLog = testParseLog logStr             -- Parser [LogLine]
        --     let ( LogOf lls2 ) = lls

        --     parsedLog `shouldBe` Success lls2


