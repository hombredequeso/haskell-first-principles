{-# LANGUAGE QuasiQuotes #-}

module Quasimodo where

-- To get this to work, the text-rawstring-qq package needs to be downloaded.
-- If using cabal, this can be done as follows:
-- > cabal update
-- > cabal install raw-strings-qq

import Text.RawString.QQ

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]
