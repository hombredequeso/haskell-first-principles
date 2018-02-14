module Phone where

import Data.List (elemIndex, find)
import Data.Char (isUpper, toLower)

type Digit = Char
type Chars = [Char]
type Presses = Int

data Button = Button Digit Chars deriving (Eq, Show)
data DaPhone = DaPhone [Button] deriving (Eq, Show)

upperCaseButtonChar = '*'
upperCaseButton = Button upperCaseButtonChar "^*"

thePhone = DaPhone [
                 Button '1' "1",
                 Button '2' "abc2",
                 Button '3' "def3",
                 Button '4' "ghi4",
                 Button '5' "jkl5",
                 Button '6' "mno6",
                 Button '7' "pqrs7",
                 Button '8' "tuv8",
                 Button '9' "wxyz9",
                 upperCaseButton,
                 Button '0' " +_0",
                 Button '#' ".,#"
            ]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"

-- Valid presses: 1 and up
--

-- Use case when needing to pattern match in order to get at pars of the response, like the x in "Just x" here
-- It's basically like inlining a function.
reverseTapsLowerCase :: DaPhone -> Char -> [(Digit, Presses)]
reverseTapsLowerCase (DaPhone []) _ = []
reverseTapsLowerCase (DaPhone ((Button digit chars):btns)) char = case elemIndex char chars of
                                                                  Just x -> [(digit, x + 1)]
                                                                  Nothing -> reverseTapsLowerCase (DaPhone btns) char

-- reverseTaps using case. Don't really need to get at parts of the response though.
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone char = 
    case (isUpper char) of
      True -> [(upperCaseButtonChar, 1)] ++ reverseTapsLowerCase phone (toLower char)
      False -> reverseTapsLowerCase phone char


-- using guards. Probably clearer in this case, and there is no need get at anything in the (isUpper char) result
reverseTapsGuard phone char 
  | (isUpper char) = [(upperCaseButtonChar, 1)] ++ reverseTapsLowerCase phone (toLower char)
  | otherwise = reverseTapsLowerCase phone char

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone message = foldr (\c acc -> reverseTaps phone c ++ acc) [] message

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined

