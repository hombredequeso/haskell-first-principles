{-# LANGUAGE OverloadedStrings #-}

module Ipv4Paraser where

import Data.Word

import Text.Trifecta
import Text.Parser.Token

import Test.Hspec
import Test.QuickCheck hiding (Success, Failure, Result)

data IPAddress =
    IPAddress Word32
    deriving (Eq, Ord)


makeLengthAtLeast :: Int -> a -> [a] -> [a]
makeLengthAtLeast minLength defaultVal  as
    | ( length as >= minLength ) = as
    | otherwise = makeLengthAtLeast minLength defaultVal ( defaultVal:as )

joinWithSeparator :: Show a => String -> [a] -> String
joinWithSeparator _ [] = ""
joinWithSeparator separator (x:[]) = (show x)
joinWithSeparator separator ( x:xs ) = (show x) ++ separator ++ (joinWithSeparator separator xs)

instance Show IPAddress where
    show (IPAddress wordAddress) = 
        let places = toBaseValues 256 (fromIntegral wordAddress) in
            let atleast4Places = makeLengthAtLeast 4 0 places in
                joinWithSeparator "." atleast4Places



parseAddress :: Parser IPAddress
parseAddress = do
    a <- natural
    _ <- char '.'
    b <- natural
    _ <- char '.'
    c <- natural
    _ <- char '.'
    d <- natural
    let i = toBaseInt 256 [a, b, c, d]
    return (IPAddress $ fromIntegral i)

toBaseInt :: Integer -> [Integer] -> Integer
toBaseInt base digits = toBaseInt' 0 0 base (reverse digits)

toBaseInt' :: Integer -> Int -> Integer -> [Integer] -> Integer
toBaseInt' total pos base [] = total
toBaseInt' total pos base (h:t) = toBaseInt' (total + (h * base ^ pos)) (pos + 1) base t

instance Eq a =>  Eq (Result  a) where
    (Success x) == (Success y) =  x == y
    (Failure x) == (Success y) = False
    (Success x) == (Failure y) = False
    (Failure x) == (Failure y) = True

toBaseValues:: Integer -> Integer -> [Integer]
toBaseValues a b = reverse $ toBaseValues' a b

toBaseValues' _ 0 = []
toBaseValues' base value =
    let (d, m) = value `divMod` base in
        m : (toBaseValues' base d)


instance Arbitrary IPAddress where
    arbitrary = IPAddress <$> arbitrary

showThenParseIsRoundTrip :: IPAddress -> Bool
showThenParseIsRoundTrip ipAddress = 
    let ipString = show ipAddress in
        let ipParsed = parseString parseAddress mempty ipString in
            ipParsed == Success ipAddress 


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

        -- it "low level hack test" $ do
        --     let ipAddress = 1
        --     let places = toBaseValues 256 ipAddress
        --     print places
        --     let atLeast4 =makeLengthAtLeast 4 0 places 
        --     print atLeast4
        --     print $ joinWithSeparator "." atLeast4


    describe "Show IPAddress -> Parse IPAddress is a round trip" $ do
        it "0.0.0.1" $ do
            let w32 = 1
            let ipAddress = IPAddress w32
            let ipAddressStr = show ipAddress
            -- print ipAddressStr
            let parsedIPAddress = parse (ipAddressStr )
            -- print parsedIPAddress
            Success ipAddress  `shouldBe` parsedIPAddress

        it "1.0.0.0" $ do
            let w32 = 1 * 256^3
            let ipAddress = IPAddress w32
            let ipAddressStr = show ipAddress
            -- print ipAddressStr
            let parsedIPAddress = parse (ipAddressStr )
            -- print parsedIPAddress
            Success ipAddress  `shouldBe` parsedIPAddress

        it "random values" $ do
            property $ \ipAddress ->
                let ipAddressStr = show ipAddress in
                    let parsedIPAddress = parse (ipAddressStr ) in
                        Success ipAddress  == parsedIPAddress


