{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Main where

import Data.List (intercalate)
import Prelude hiding (elem, sum, any)
import qualified Prelude as P
import Control.Arrow
import Control.Lens
import Data.Sequence.Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Foldable
import System.Random
import Debug.Trace
import System.Environment

data Decision = GoUp
              | GoDown
              | Load
              | Unload
              | Wait
              deriving (Show, Eq)

type Floor = Int
type Prob = Float
type Elevator = Building -> Decision

data Building = Building
                { _queue :: Seq Floor
                , _numFloors :: Int
                , _floors :: [Seq Int]
                , _curPosition :: Int
                , _peopleInElevator :: Int
                , _waitSum :: Int
                , _peopleSum :: Int
                } deriving (Show)

makeLenses ''Building

-- Follows the queue exactly.
dumbElevator :: Elevator
dumbElevator b
  | hasPeople b = if inBasement b then Unload else GoDown
  | otherwise = case (== b ^. curPosition) `fmap` (b ^. queue ^? _head) of
        Just True  -> Load
        Just False -> GoUp
        Nothing    -> Wait


niceElevator :: Elevator
niceElevator b
  | hasPeople b = if inBasement b
                    then Unload
                    else if peopleOnCurrentFloor b then Load else GoDown
  | otherwise = dumbElevator b


smartElevator :: Elevator
smartElevator b
  | hasPeople b = niceElevator b
  | any (> b ^. curPosition) (b ^. queue) = GoUp
  | peopleOnCurrentFloor b = Load
  | otherwise = Wait

-- |Create new building with given number of floors.
--
mkBuilding :: Floor -> Building
mkBuilding n = Building S.empty n (replicate n S.empty) (-1) 0 0 0

-- |Check if there are people in the elevator.
--
hasPeople :: Building -> Bool
hasPeople b = b ^. peopleInElevator > 0

-- |Check if elevator is in the basement.
--
inBasement :: Building -> Bool
inBasement b = b ^. curPosition == -1

-- |Check if there are people waiting on the current floor.
--
peopleOnCurrentFloor :: Building -> Bool
peopleOnCurrentFloor b = not $ S.null $ b ^. (floors . ix (b ^. curPosition))

-- |Generate a list of `n` random values and return it with a new generator.
--
randomList :: (RandomGen g, Random a) => g -> Int -> ([a], g)
randomList g 0 = ([], g)
randomList g n = let (vs, g') = randomList g (n-1)
                     (v, g'') = random g'
                 in (v:vs, g'')

-- |Add item at the end of a `Data.Sequence` iff it is not yet contained in the
-- sequence.
--
enqueue :: (Eq a) => a -> Seq a -> Seq a
enqueue x s = if x `elem` s then s else s |> x

-- |For each floor flip a coin and generate a person with given probability.
-- The person is added to the floor and queue is also updated.
--
generatePeople :: RandomGen g => g -> Building -> Prob -> Int -> (Building, g)
generatePeople g b prob t =
    let (bools, g') = first (map (< prob)) (randomList g (b ^. numFloors))
    in (foldl' helper b (map fst $ filter snd $ zip [0..] bools), g')
  where
    helper b i = b & over queue (enqueue i)
                   & over (floors . ix i) (|> t)

load :: Int -> Building -> Building
load t b = b & peopleInElevator +~ S.length waiting
             & peopleSum +~ S.length waiting
             & waitSum +~ sum (fmap (t-) waiting)
             & over queue (S.filter (/= curFloor))
             & (floors . ix curFloor) .~ S.empty
  where
    curFloor = b ^. curPosition
    waiting = b ^. (floors . ix curFloor)

unload :: Building -> Building
unload b = b & peopleInElevator .~ 0

runSimulation :: RandomGen g => Elevator -> g -> Int -> Prob -> Building -> Building
runSimulation elevator g' niters prob b' = fst $ worker 0 g' b'
  where
    worker :: RandomGen g => Int -> g -> Building -> (Building, g)
    worker n g b
        | n == niters = (b, g)
        | otherwise = let (b', g') = generatePeople g b prob n
                          decision = elevator b'
                      in case decision of
                        Wait   -> worker (n+1) g' b'
                        GoUp   -> worker (n+1) g' (b' & curPosition +~ 1)
                        GoDown -> worker (n+1) g' (b' & curPosition -~ 1)
                        Load   -> worker (n+1) g' (load n b')
                        Unload -> worker (n+1) g' (unload b')

peopleLeft :: Building -> Int
peopleLeft = P.sum . map sum . (^. floors)

run :: Elevator -> Prob -> Int -> IO (Int, Int, Double, Int)
run e prob nf = do
    gen <- newStdGen
    let b = runSimulation e gen 10000 prob (mkBuilding nf)
    let avg = fromIntegral (b ^. waitSum) / fromIntegral (b ^. peopleSum)
    return (b ^. waitSum, b ^. peopleSum, avg, peopleLeft b)

main :: IO ()
main = do
    [e,nf] <- getArgs
    let elevator = case e of "dumb"  -> dumbElevator
                             "nice"  -> niceElevator
                             "smart" -> smartElevator
                             _        -> error "Unknown elevator"
    {-
    putStrLn "Prob,TotalWait,PeopleTransported,AvgWait,PeopleLeft"
    forM_ (take 400 $ iterate (+0.001) 0.01) $ \prob -> do
        (ws, ps, avg, lf) <- run elevator prob (read nf)
        putStrLn $ intercalate "," [show prob, show ws, show ps, show avg, show lf]
        -}
    putStrLn "Floors,TotalWait,PeopleTransported,AvgWait,PeopleLeft"
    forM_ [3..20] $ \f -> do
        (ws, ps, avg, lf) <- run elevator (read nf) f
        putStrLn $ intercalate "," [show f, show ws, show ps, show avg, show lf]
