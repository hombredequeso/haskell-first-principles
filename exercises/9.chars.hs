import Data.Char

capFirst::String -> String
capFirst "" = ""
capFirst (x:xs) = toUpper x : xs


capAll::String -> String
capAll "" = ""
capAll (x:xs) = toUpper x : (capAll xs)

getCapFirst::String -> Char
getCapFirst "" = undefined
getCapFirst s = toUpper (head s)

getCapFirst2 = toUpper . head
