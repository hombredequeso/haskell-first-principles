{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LexersAndParsers where

import Control.Applicative
import Data.ByteString (ByteString)
-- import Data.Char (isAlpha)
-- import Data.Map (Map)
-- import qualified Data.Map as M
-- import Data.Text (Text)
-- import qualified Data.Text.IO as TIO
-- import Test.Hspec
import Text.RawString.QQ
-- parsers 0.12.3, trifecta 1.5.2
import Text.Trifecta


digit' :: Parser Char
digit' = digit

-- Go here:
--https://hackage.haskell.org/package/parsers-0.12.8/docs/Text-Parser-Token.html
-- and read the definition of token, it will help a lot
--          token :: m a -> m a

main:: IO()
main = do
    -- digit :: CharParsing m => m Char
    print $ 
        parseString digit' mempty "123 456"
    -- some digit :: CharParsing f => f [Char]
    print $ 
        parseString (some digit') mempty "123 456"

    -- some :: Alternative f => f a -> f [a]
    -- some (some digit) :: CharParsing f => f [[Char]]
    print $ 
        parseString (some (some digit)) mempty "123 456"

    -- print $
    --     parseString (some integer) mempty " 123\n\n 456"

    -- 'integer' is already a token parser, so both of these are the same:
    --      token integer
    --      integer
    print $
        parseString (whiteSpace >> (some (token integer))) mempty " 123\n\n 456"
    print $
        parseString (whiteSpace >> (some integer)) mempty " 123\n\n 456"

    print $
        parseString (whiteSpace >> (some (token digit))) mempty " 123\n\n 456"
