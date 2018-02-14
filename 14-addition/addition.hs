module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
              | n < d = (count, n)
              | otherwise =
                  go (n - d) d (count + 1)

add1 :: Num a => a -> a
add1 x = x + 1

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x


runQc :: IO ()
runQc = quickCheck prop_additionGreater

runQc2 :: IO ()
runQc2 = quickCheck prop_additionGreater

type MyString = String

data XString = XString' String deriving (Eq, Show)

getStr :: XString -> String
getStr (XString' s) = s

genXString :: Gen XString
genXString = return (XString' "xxxxx")


genXString2 :: Gen XString
genXString2 = do
    s <- arbitrary
    return (XString' s)

instance Arbitrary XString where
    arbitrary = do
        s <- arbitrary
        return $ XString' s

main :: IO ()
main = 
    hspec $ do
        describe "Addition" $ do
            it "1 + 1 is greater than 1" $ do
                (1 + 1) > (1::Integer) `shouldBe` True
            it "x + 1 is always greater than x" $
                property $ \x -> x + 1 > (x :: Int)

        describe "dividedBy" $ do
            it "15 divided by 3 is 5" $ do
                dividedBy (15::Integer) 3 `shouldBe` (5, 0)
            it "22 divided by 5 is 4 remainder 2" $ do
                dividedBy (22::Integer) 5 `shouldBe` (4, 2)

        describe "add1" $ do
            it "add1 gives a higher value" $
                property $ \x -> add1 x > (x :: Int)

        describe "XString" $ do
            it "equals itself" $
                property $ \x -> x == (x::XString)
            it "is created by a string ok" $
                property $ \s -> getStr (XString' s)  == s


