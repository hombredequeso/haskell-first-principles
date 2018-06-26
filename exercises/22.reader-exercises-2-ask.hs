-- Implement the following function. If you get stuck, remember it’s
-- less complicated than it looks.
--          ! Of course it is. It always looks simple when its there !

-- For a bit more help, see:
-- https://stackoverflow.com/a/14179721

newtype Reader r a =
    Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader (\x -> x)
