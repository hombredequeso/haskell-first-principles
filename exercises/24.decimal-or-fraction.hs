module DecimalOrFraction where

import Control.Applicative
import Text.Trifecta
import Data.Ratio ((%))


parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

parseFraction' = parseString parseFraction mempty

parseDecimal :: Parser Integer
parseDecimal = decimal

type DecimalOrFraction = Either Rational Integer

parseDecimalOrFraction :: Parser DecimalOrFraction
parseDecimalOrFraction = 
    ( Left <$> try parseFraction ) 
    <|> ( Right <$> parseDecimal ) 
-- parseDecimalOrFraction = ( Left <$> parseFraction ) 
-- parseDecimalOrFraction = ( Left <$> try parseFraction ) 
-- parseDecimalOrFraction = ( Right <$> parseDecimal ) 

-- :t parseDecimal
-- parseDecimal :: Parser Integer
--
-- :t Right parseDecimal
-- Right parseDecimal :: Either a (Parser Integer)
--
-- :t Right <$> parseDecimal
-- Right <$> parseDecimal :: Parser (Either a Integer)
--
-- :t (<|>)
-- (<|>) :: Alternative f => f a -> f a -> f a


parseDOrF = parseString parseDecimalOrFraction mempty

main :: IO()
main = do
    print $ parseFraction' "1/9"

