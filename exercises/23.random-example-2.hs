module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

import qualified Data.Map.Strict as Map


data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
      1 -> DieOne
      2 -> DieTwo
      3 -> DieThree
      4 -> DieFour
      5 -> DieFive
      6 -> DieSix
      x ->
          error $
              "intToDie got non 1-6 integer: "
            ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    let die = intToDie n
    return (die, s)

-- > :t rollDie'
-- rollDie' :: State StdGen Die
--
-- > :i state
--state :: Monad m => (s -> (a, s)) -> StateT s m a

-- > :i randomR
-- class Random a where
--   randomR :: RandomGen g => (a, a) -> g -> (a, g)

-- > :t (randomR(1,6))
-- (randomR(1,6)) :: (Num a, RandomGen g, Random a) => g -> (a, g)

-- intToDie :: Int -> Die
-- a :: Int
-- g, carrying the state, is used to give the next a (Int)
-- This Int is then mapped into a Die by intToDie
-- g will be RandomGen

-- class RandomGen g where
--   next :: g -> (Int, g)
--   genRange :: g -> (Int, Int)
--   split :: g -> (g, g)
--   {-# MINIMAL next, split #-}
--         -- Defined in `System.Random'
-- instance RandomGen StdGen -- Defined in `System.Random'

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie' rollDie' rollDie'

-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d


-- Or, done monadically using do notation
rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = do
    a <- rollDie'
    b <- rollDie'
    c <- rollDie'
    return (a,b,c)

-- Or, done monadically without do:
rollDieThreeTimes'' :: State StdGen (Die, Die, Die)
rollDieThreeTimes'' = 
    rollDie' >>= (\a -> 
        rollDie' >>= (\b -> 
            rollDie' >>= (\c -> 
                return (a, b, c))))

-- Or, done applicatively, without lift
rollDieThreeTimes''' :: State StdGen (Die, Die, Die)
rollDieThreeTimes''' = (,,) <$> rollDie' <*> rollDie' <*> rollDie'

-- And, if you wanted the same thing as random-example.hs,
-- it would be necessary to provide the state somehow.
rollDieThreeTimesX :: (Die, Die, Die)
rollDieThreeTimesX =  evalState rollDieThreeTimes (mkStdGen 9)
-- or, using runState need to add in fst to get the result minus the state
-- rollDieThreeTimesX = fst ( runState rollDieThreeTimes (mkStdGen 9) )

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

-- imported by: 
-- import Control.Monad (replicateM)
-- replicateM :: Monad m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
                  -- go :: Int -> Int -> StdGen -> Int
                  -- go sum count gen
rollsToGetTwenty g = go 0   0     g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen


rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go n (0, [])   0     g
    where
        go :: Int -> (Int, [Die]) -> Int -> StdGen -> (Int, [Die])
        go limit (sum, rolls) count gen
          | sum >= limit = (count, rolls)
          | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
             in go limit (sum + die, intToDie die : rolls) (count + 1) nextGen

-- rollsCountLogged 100 . mkStdGen <$> randomIO
 
-- :t (rollsCountLogged 100)
-- (rollsCountLogged 100) :: StdGen -> (Int, [Die])

-- :t mkStdGen
-- mkStdGen :: Int -> StdGen

-- :t (rollsCountLogged 100 . mkStdGen)
-- (rollsCountLogged 100 . mkStdGen) :: Int -> (Int, [Die])

-- :t (rollsCountLogged 100 . mkStdGen <$> randomIO)
-- (rollsCountLogged 100 . mkStdGen <$> randomIO) :: IO (Int, [Die])

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go n 0   0     g
    where
        go :: Int -> Int -> Int -> StdGen -> Int
        go limit sum count gen
          | sum >= limit = count
          | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go limit (sum + die) (count + 1) nextGen

-- challenge: collect stats on the roll counts.

-- perform some number of rollsToN, and collect stats on how many times each count occurs
statsForRollsToN :: Int -> Int -> Map.Map Int Integer -> StdGen -> Map.Map Int Integer
statsForRollsToN n 0 m stdGen = m
statsForRollsToN n sample m stdGen = statsForRollsToN n (sample-1) (Map.insertWithKey (\key next old -> next + old) rolls 1 m) stdGen where
    rolls = rollsToGetN n stdGen

-- the third Int is used to create a StdGen (mkStdGen)
statsForRollsToN' :: Int -> Int -> Int -> Map.Map Int Integer
statsForRollsToN' n sample = statsForRollsToN n sample Map.empty . mkStdGen 

-- main :: IO ()
-- main = do
--     print $ rollDieThreeTimes


