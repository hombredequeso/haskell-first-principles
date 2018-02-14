dropLast n s =
    take toTake s where
        toTake = (length s - n)

dropLast2 n s =
    let toTake = (length s - n) in
        take toTake s 

getCharStr n s =
    take 1 (drop (n-1) s)


tailN :: Int -> [a] -> [a]
tailN n s =
    drop (length s - n) s

thirdLetter:: [a] -> a
thirdLetter s =
    head (drop 3 s)

letterIndex :: Int -> [a] -> a
letterIndex n s =
    head (drop n s)


