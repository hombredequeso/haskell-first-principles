{-# LANGUAGE OverloadedStrings #-}

module Ipv4Paraser where

import Data.Word
import Text.Trifecta
import Text.Parser.Token

import Test.Hspec
import Test.QuickCheck hiding (Success, Failure, Result) -- Clashes with Text.Trifecta

data IPAddress =
    IPAddress Word32
    deriving (Eq, Ord)

instance Show IPAddress where
    show (IPAddress wordAddress) = joinWithSeparator "." atleast4Places where
            places = toBaseValues 256 (fromIntegral wordAddress)
            atleast4Places = makeLengthAtLeast 4 0 places

-- Same as above, but using notation: Let ... in ...
-- instance Show IPAddress where
--     show (IPAddress wordAddress) = 
--         let places = toBaseValues 256 (fromIntegral wordAddress) in
--             let atleast4Places = makeLengthAtLeast 4 0 places in
--                 joinWithSeparator "." atleast4Places

instance Arbitrary IPAddress where
    arbitrary = IPAddress <$> arbitrary


makeLengthAtLeast minLength defaultVal  as
    | ( length as >= minLength ) = as
    | otherwise = makeLengthAtLeast minLength defaultVal ( defaultVal:as )

-- joining with recursion - replaces for joining with foldr
-- joinWithSeparator1 :: Show a => String -> [a] -> String
-- joinWithSeparator1 _ [] = ""
-- joinWithSeparator1 separator (x:[]) = (show x)
-- joinWithSeparator1 separator ( x:xs ) = 
--     (show x) ++ separator ++ (joinWithSeparator separator xs)

join' :: Show a => String -> a -> String -> String
join' separator a b = if (length b == 0) then show a else show a ++ separator ++ b

joinWithSeparator :: Show a => String -> [a] -> String
joinWithSeparator separator = foldr ( join' separator ) ""


-- Simplest to read version.
-- But with introduction of parseNWithSeparator, which could be reused,
-- the new version of parseAddress is better.
parseAddress2 :: Parser IPAddress
parseAddress2 = do
    a <- natural
    _ <- char '.'
    b <- natural
    _ <- char '.'
    c <- natural
    _ <- char '.'
    d <- natural
    let i = toBaseInt 256 [a, b, c, d]
    return (IPAddress $ fromIntegral i)

parseAddress :: Parser IPAddress
parseAddress = 
    IPAddress . fromIntegral . ( toBaseInt 256 ) <$> 
        (parseNWithSeparator natural (char '.') 4)

parseNWithSeparator :: Parser a -> Parser sep -> Int -> Parser [a]
parseNWithSeparator _ _ 0 = pure []
parseNWithSeparator aParser _ 1 = sequenceA [aParser]
parseNWithSeparator aParser sepParser count = do
    aa <- aParser
    _ <- sepParser
    remainder <- parseNWithSeparator aParser sepParser (count - 1)
    return (aa : remainder)

-- Or if you don't like 'do' notation :-), for the last one:
parseNWithSeparator2 :: Parser a -> Parser sep -> Int -> Parser [a]
parseNWithSeparator2 aParser sepParser count = 
    aParser >>= \aa -> 
    sepParser >> 
    (parseNWithSeparator aParser sepParser (count - 1)) >>= \as -> 
    return (aa : as)

toBaseInt :: Integer -> [Integer] -> Integer
toBaseInt base digits = toBaseInt' 0 0 base (reverse digits)

toBaseInt' :: Integer -> Int -> Integer -> [Integer] -> Integer
toBaseInt' total pos base [] = total
toBaseInt' total pos base (h:t) = toBaseInt' (total + (h * base ^ pos)) (pos + 1) base t

toBaseValues:: Integer -> Integer -> [Integer]
toBaseValues a b = reverse $ toBaseValues' a b

toBaseValues' _ 0 = []
toBaseValues' base value =
    let (d, m) = value `divMod` base in
        m : (toBaseValues' base d)

showThenParseIsRoundTrip :: IPAddress -> Bool
showThenParseIsRoundTrip ipAddress = Success ipAddress == parsedIPAddress  where
    ipString = show ipAddress
    parsedIPAddress = parseString parseAddress mempty ipString

instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True

main :: IO ()
main = hspec $ do

    describe "toBaseValues" $ do
        it "results in decimal place values for base 10" $ do
            toBaseValues 10 123 `shouldBe` [1,2,3]

    describe "toBaseInt" $ do
        it "converts correctly to base 10" $ do
            toBaseInt 10 [1, 2, 3] `shouldBe` 123
        it "converts base 256 correctly" $ do
            toBaseInt 256 [172, 16, 254, 1]  `shouldBe` 2886794753
            toBaseInt 256 [204, 120, 0, 15]  `shouldBe` 3430416399

    let parse = parseString parseAddress mempty

    describe "User Specification: " $ do
        it "172.16.254.1 -> 2886794753" $ do
            let s = "172.16.254.1"
            let ipAddress = parse s
            ipAddress `shouldBe` Success (IPAddress 2886794753)

        it "204.120.0.15 -> 3430416399" $ do
            let s = "204.120.0.15"
            let ipAddress = parse s
            ipAddress `shouldBe` Success (IPAddress 3430416399)


    describe "Show IPAddress" $ do

        it "produces a formatted IP Address" $ do
            let s = "204.120.0.15"
            (show $ IPAddress 3430416399) `shouldBe` s


    describe "Show IPAddress -> Parse IPAddress is a round trip" $ do
        it "0.0.0.1" $ do
            let w32 = 1
            let ipAddress = IPAddress w32
            let ipAddressStr = show ipAddress
            let parsedIPAddress = parse (ipAddressStr )
            Success ipAddress  `shouldBe` parsedIPAddress

        it "1.0.0.0" $ do
            let w32 = 1 * 256^3
            let ipAddress = IPAddress w32
            let ipAddressStr = show ipAddress
            let parsedIPAddress = parse (ipAddressStr )
            Success ipAddress  `shouldBe` parsedIPAddress

        it "Round trip is equal: IPAddress -> string -> Parser<IPAddress>" $ do
            property $ \ipAddress ->
                let ipAddressStr = show ipAddress in
                    let parsedIPAddress = parse (ipAddressStr ) in
                        Success ipAddress  == parsedIPAddress

        it "Round trip is equal: IPAddress -> string -> Parser<IPAddress> : using function" $ do
            property showThenParseIsRoundTrip

    describe "joinWithSeparator" $ do
        it "no elements produces empty string" $ do
            let noElements = [] :: [ String ]
            joinWithSeparator "**" noElements `shouldBe` ""

        it "single element produces string without separator" $ do
            joinWithSeparator "**" [1] `shouldBe` "1"

        it "multiple elements produces string with separators" $ do
            joinWithSeparator "**" [1,2,3] `shouldBe` "1**2**3"

