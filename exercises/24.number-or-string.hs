module NumberOrString where

import Control.Applicative
import Text.Trifecta

-- eitherOr is defined in here:
import Quasimodo

type NumberOrString =
    Either Integer String

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = 
    (Left <$> integer) 
    <|> (Right <$> some letter)

parseNosWithNewLines :: Parser NumberOrString
parseNosWithNewLines = do
    skipMany (oneOf "\n")
    numberOrString <- parseNumberOrString
    skipMany (oneOf "\n")
    return numberOrString

main = do
    let p f i = parseString f mempty i
    print $ p parseNosWithNewLines eitherOr
