module SemVer where


import Control.Applicative
import Text.Trifecta

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
parseValidChar :: Parser Char
parseValidChar = upper <|> lower <|> digit <|> (char '-')
testParseValidChar = parseString parseValidChar mempty

parseReleaseStr :: Parser [Char]
parseReleaseStr = some parseValidChar
testParseReleaseStr = parseString parseReleaseStr mempty

parseNos :: Parser NumberOrString
parseNos = ( NOSI <$> integer ) 
             <|> ( NOSS <$> parseReleaseStr ) 
testParseNos = parseString parseNos mempty

parseRelease :: Parser [NumberOrString]
parseRelease = do
    x <- parseNos
    xs <- many (( char '.' ) >> parseNos)
    return (x:xs)

testParseRelease = parseString parseRelease mempty

parseReleaseSeparator :: Parser Char
parseReleaseSeparator = char '-'
testparseReleaseSeparator = parseString parseReleaseSeparator mempty

parseMetadataSeparator :: Parser Char
parseMetadataSeparator = char '+'

toEmpty :: Maybe [t] -> [t]
toEmpty ( Just x ) = x
toEmpty Nothing = []

parseReleaseM :: Parser (Maybe [NumberOrString])
parseReleaseM = optional (parseReleaseSeparator >> parseRelease)

parseMetadataM :: Parser (Maybe [NumberOrString])
parseMetadataM = optional (parseMetadataSeparator >> parseRelease)

parseReleaseX = toEmpty <$> parseReleaseM
testParseReleaseX =  parseString parseReleaseX mempty 

parseMetadataX = toEmpty <$> parseMetadataM

parseSemVer :: Parser SemVer
parseSemVer = SemVer 
                <$> integer 
                <*> ((char '.') >> integer) 
                <*> ((char '.') >> integer) 
                <*> parseReleaseX
                <*> parseMetadataX

testParseSemVer =  parseString parseSemVer mempty

-- working examples:
-- *SemVer> testParseSemVer "1.2.3-rere.r3r3.333+432.432.r3r3"
-- Success (SemVer 1 2 3 [NOSS "rere",NOSS "r3r3",NOSI 333] [NOSI 432,NOSI 432,NOSS "r3r3"])
-- *SemVer> testParseSemVer "1.2.3+432.432.r3r3"
-- Success (SemVer 1 2 3 [] [NOSI 432,NOSI 432,NOSS "r3r3"])
-- *SemVer> testParseSemVer "1.2.3"
-- Success (SemVer 1 2 3 [] [])
-- *SemVer> testParseSemVer "1.2.3-54.654.t4t4.helloWorld"
-- Success (SemVer 1 2 3 [NOSI 54,NOSI 654,NOSS "t4t4",NOSS "helloWorld"] [])


