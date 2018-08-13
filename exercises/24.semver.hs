{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SemVer where

import Control.Applicative
import Text.Trifecta

import Test.Hspec

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString =
    NOSS String
  | NOSI Integer
    deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
    SemVer 
        Major 
        Minor 
        Patch 
        Release 
        Metadata
    deriving (Eq, Ord, Show)

-- =========================================================
-- Let the parsers begin...
--
parseSemVerChar :: Parser Char
parseSemVerChar = upper <|> lower <|> digit <|> (char '-')

parseSemVerStr :: Parser [Char]
parseSemVerStr = some parseSemVerChar

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = 
    ( NOSI <$> integer ) 
    <|> ( NOSS <$> parseSemVerStr ) 

parseNumberOrStrings :: Parser [NumberOrString]
parseNumberOrStrings = do
    x <- parseNumberOrString
    xs <- many (( char '.' ) >> parseNumberOrString)
    return (x:xs)

toEmpty :: Maybe [t] -> [t]
toEmpty ( Just x ) = x
toEmpty Nothing = []

parseLabel :: Char -> Parser [NumberOrString]
parseLabel separatorChar = 
    toEmpty <$> 
        optional ((char separatorChar) >> parseNumberOrStrings)

parseSemVer :: Parser SemVer
parseSemVer = SemVer 
                <$> integer 
                <*> ((char '.') >> integer) 
                <*> ((char '.') >> integer) 
                <*> parseLabel '-'
                <*> parseLabel '+'


instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True


-- working examples:
-- *SemVer> testParseSemVer "1.2.3-rere.r3r3.333+432.432.r3r3"
-- Success (SemVer 1 2 3 [NOSS "rere",NOSS "r3r3",NOSI 333] [NOSI 432,NOSI 432,NOSS "r3r3"])
-- *SemVer> testParseSemVer "1.2.3+432.432.r3r3"
-- Success (SemVer 1 2 3 [] [NOSI 432,NOSI 432,NOSS "r3r3"])
-- *SemVer> testParseSemVer "1.2.3"
-- Success (SemVer 1 2 3 [] [])
-- *SemVer> testParseSemVer "1.2.3-54.654.t4t4.helloWorld"
-- Success (SemVer 1 2 3 [NOSI 54,NOSI 654,NOSS "t4t4",NOSS "helloWorld"] [])


main :: IO ()
main = hspec $ do

let testParseSemVer =  parseString parseSemVer mempty

describe "parseSemVer" $ do
    it "can parse Major.Minor.Patch" $ do
        let semVer = "1.2.3"
        let parsedSemVer = testParseSemVer semVer
        -- print semVer
        -- print parsedSemVer
        
        parsedSemVer `shouldBe` ( Success ( SemVer 1 2 3 [] [] ) )

    it "can parse release and metadata" $ do
        let semVer = "1.2.3-rere.r3r3.333+432.432.r3r3"
        let parsedSemVer = testParseSemVer semVer
        parsedSemVer `shouldBe` Success (SemVer 1 2 3 [NOSS "rere",NOSS "r3r3",NOSI 333] [NOSI 432,NOSI 432,NOSS "r3r3"])

