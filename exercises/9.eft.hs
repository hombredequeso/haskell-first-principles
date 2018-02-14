
go2 start end total
    | start > end = total
    | otherwise = start : (go2 (succ start) end total)

eftBool :: Bool -> Bool -> [Bool]
eftBool = undefined

eftOrd :: Ordering
            -> Ordering
            -> [Ordering]
eftOrd = undefined

eftInt :: Int -> Int -> [Int]
eftInt start end = go2 start end []
                    -- where
                    --     go start end total
                    --         | start > end = total
                    --         | otherwise = start : (go (succ start) end total)


eftChar :: Char -> Char -> [Char]
eftChar start end = go2 start end []
--  eftChar = undefined
