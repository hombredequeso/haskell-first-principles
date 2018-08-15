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
    deriving (Eq, Show)

instance Ord NumberOrString where
    compare (NOSI i) (NOSI i') = compare i i'
    compare (NOSS s) (NOSS s') = compare s s'
    compare (NOSI _) (NOSS _) = GT

data VersionNumber = VersionNumber Integer

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
    deriving (Eq, Show)


instance Ord SemVer where
    compare 
        (SemVer major1 _ _ _ _) 
        (SemVer major2 _ _ _ _) | major1 /= major2 = compare major1 major2
    compare 
        (SemVer major1 minor1 _ _ _) 
        (SemVer major2 minor2 _ _ _) | ( major1 == major2 ) && (minor1 /= minor2) = compare minor1 minor2
    compare 
        (SemVer major1 minor1 patch1 _ _) 
        (SemVer major2 minor2 patch2 _ _) 
          | ( major1 == major2 ) && (minor1 == minor2) && (patch1 /= patch2)= compare patch1 patch2
    -- TODO: if there is a pre-release, it makes it lesser than an equal one without pre-release.
    
    compare 
        (SemVer major1 minor1 patch1 [] _) 
        (SemVer major2 minor2 patch2 [] _) 
          | ( major1 == major2 ) && (minor1 == minor2) && (patch1 == patch2) = EQ 

    compare 
        (SemVer major1 minor1 patch1 r1 _) 
        (SemVer major2 minor2 patch2 [] _) 
          | ( major1 == major2 ) && (minor1 == minor2) && (patch1 == patch2) = GT

    compare 
        (SemVer major1 minor1 patch1 r1 _) 
        (SemVer major2 minor2 patch2 r2 _) 
          | ( major1 == major2 ) && (minor1 == minor2) && (patch1 == patch2) = compare r1 r2

    compare _ _  = EQ

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

    -- it "must have a patch release numberr" $ do
    --     let semVer = "1.2"
    --     let parsedSemVer = testParseSemVer semVer
    --     parsedSemVer `shouldBe` Failure _ 

describe "NumberOrString ordinality" $ do
    it "Integers are numerically compared" $ do
        let a = NOSI 2
        let b = NOSI 1
        (compare a b) `shouldBe` GT

    it "Strings are numerically lexically" $ do
        let alpha = NOSS "alpha"
        let beta = NOSS "beta"
        (compare beta alpha) `shouldBe` GT

    it "Integers are greater than strings" $ do
        let int = NOSI 1
        let str = NOSS "alpha"
        (compare int str) `shouldBe` GT


    it "An element is greater than no element" $ do
        let a = [NOSS "alpha", NOSI 1]
        let b = [NOSS "alpha"]
        (compare a b) `shouldBe` GT

describe "parseSemVer ordinality" $ do
    it "2.0.0 > 1.0.0" $ do
        let v2 = testParseSemVer "2.0.0"
        let v1 = testParseSemVer "1.0.0"
        (compare <$> v2 <*> v1) `shouldBe` Success (GT)

    it "1.1.0 > 1.0.0" $ do
        let v11 = testParseSemVer "1.1.0"
        let v1 = testParseSemVer "1.0.0"
        (compare <$> v11 <*> v1) `shouldBe` Success (GT)


    it "1.0.1 > 1.0.0" $ do
        let v101 = testParseSemVer "1.0.1"
        let v1 = testParseSemVer "1.0.0"
        (compare <$> v101 <*> v1) `shouldBe` Success (GT)

    it "1.0.0 > 1.0.0-alpha" $ do
        let v1 = testParseSemVer "1.0.0"
        let v1alpha = testParseSemVer "1.0.0-alpha"
        (compare <$> v1alpha <*> v1) `shouldBe` Success (GT)

    it "1.0.0 = 1.0.0" $ do
        let v1 = testParseSemVer "1.0.0"
        let v1' = testParseSemVer "1.0.0"
        (compare <$> v1 <*> v1') `shouldBe` Success (EQ)

    it "1.0.0-2 > 1.0.0-1" $ do
        let a = testParseSemVer "1.0.0-2"
        let a' = testParseSemVer "1.0.0-1"
        (compare <$> a <*> a') `shouldBe` Success (GT)


    it "1.0.0-alpha2 > 1.0.0-alpha1" $ do
        let a = testParseSemVer "1.0.0-alpha2"
        let a' = testParseSemVer "1.0.0-alpha1"
        (compare <$> a <*> a') `shouldBe` Success (GT)


    it "1.0.0-alpha.2 > 1.0.0-alpha.1" $ do
        let a = testParseSemVer "1.0.0-alpha.2"
        let a' = testParseSemVer "1.0.0-alpha.1"
        (compare <$> a <*> a') `shouldBe` Success (GT)

    it "metadata does not affect the ordering" $ do
        let v1 = testParseSemVer "1.0.0-alpha+1"
        let v1' = testParseSemVer "1.0.0-alpha+2"
        (compare <$> v1 <*> v1') `shouldBe` Success (EQ)

