module LearnParsers where

import Control.Applicative
import Text.Trifecta

-- Note, if loading this file results in the error:
--      Could not find module `Text.Trifecta'
-- then do the following (on a regular command line)
-- > cabal update
-- > cabal install trifecta

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
-- one ::  CharParsing m => m Char
one :: Parser Char
one = char '1'

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop

oneEof = one >> eof

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two characters,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop

oneTwoEof = oneTwo >> eof

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

pNL s =
    putStrLn ('\n' : s)


oneS :: Parser String
oneS = string "1"

oneTwoS :: Parser String
oneTwoS = string "12"

oneTwoThreeS :: Parser String
oneTwoThreeS = string "123"

oneTwoOrThree :: Parser String
oneTwoOrThree = choice [ oneTwoThreeS, oneTwoS, oneS ]

oneTwoOrThree' :: Parser String
oneTwoOrThree' = oneS <|> oneTwoS <|> oneTwoThreeS

-- parseString oneTwoOrThree mempty "123456"

oneTwoOrThreeEof = oneTwoOrThree >> eof


type NumberOrString =
    Either Integer String


parseNos :: Parser NumberOrString
parseNos =
        (Left <$> integer)
    <|> (Right <$> some letter)

main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'


main2 = do
    let a = "blah"
    let b = "123"
    let c = "123blah789"
    let p f i =
         parseString f mempty i
    print $ p (some letter) a
    print $ p integer b
    print $ p parseNos a
    print $ p parseNos b
    print $ p (many parseNos) c
    print $ p (some parseNos) c
