{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    -- [2] [1]
    char '/'
    -- [3]
    denominator <- decimal

    -- [ 4 ]
    return (numerator % denominator)

parseFraction' :: Parser Rational
parseFraction' = decimal >>= (\x -> 
                    (char '/') >>= (\_ -> 
                        decimal  >>= \z ->
                            return (x % z )))


main :: IO ()
main = do
    let parseFraction' = parseString parseFraction mempty
    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' alsoBad
    print $ parseFraction' badFraction

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
    let virtuousFraction' =
         parseString virtuousFraction mempty
    print $ virtuousFraction' badFraction
    print $ virtuousFraction' alsoBad
    print $ virtuousFraction' shouldWork
    print $ virtuousFraction' shouldAlsoWork

parseAndReturnInt :: Parser Integer
parseAndReturnInt = do
        i <- decimal
        eof
        return i
-- parseAndReturnInt = decimal >>= (\x -> eof >>= (\_ -> return x))

