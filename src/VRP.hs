{- |
Copyright    : 2014 Tomáš Musil
License      : BSD-3
Stability    : experimental
Portability  : portable 

Vehicle Routing Problem.

-}

module VRP
  ( -- * Types
    Size
  , Vertex
  , Path
  , Distance
  , Coordinate
  , Capacity
  , Demand
  , Solution
  , State
  , Score
  , DistF
  , DemF
    -- * Array types
  , DArray
    -- * Data manipulation
  , readProblem
    -- * Problem formulation
  , neighbour
  , solve
  , allNeigh
  ) where

import Control.Applicative
import Control.Monad.Random
import Data.Array.Unboxed
import System.IO
import qualified Data.Map as M
import Data.List
import Data.List.Split

type Vertex = Int
type Path = [Vertex]
type Size = Int
type Score = Double
type Solution = (Score, (State, [Path]))
type Distance = Double
type Coordinate = Double
type CoArray = UArray Vertex Coordinate
type Demand = Int
type Capacity = Demand
type DArray = UArray Vertex Demand
type DistF = (Vertex, Vertex) -> Distance
type DemF = Vertex -> Demand
type State = M.Map Int Vertex

readProblem :: FilePath -> IO (Size, Size, Capacity, DistF, DemF)
readProblem filePath = withFile filePath ReadMode $ \ file -> do
  [n, v, c] <- map read . words <$> hGetLine file 
  [dem, xs, ys] <- transpose . map words . take n . lines <$> hGetContents file
  let distF = distC (listArray (0, n - 1) $ map read xs, listArray (0, n - 1) $ map read ys)
      demF = (!) (listArray (0, n - 1) $ map read dem :: DArray)
  return (n, v, c, distF, demF)
  --dist ! (n, n) `seq` return (n, dist)

distC :: (CoArray, CoArray) -> (Vertex, Vertex) -> Distance
distC (s, d) (a, b) = euc2 (s ! a) (d ! a) (s ! b) (d ! b)

euc2 :: Coordinate -> Coordinate -> Coordinate -> Coordinate -> Distance
euc2 x1 y1 x2 y2 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

score :: DistF -> DemF -> Capacity -> [Path] -> Score
score distF demF c sol = sum $ dist ++ penal
	where 	penal = map (fromIntegral . (* 100) . max 0 . (\x -> x - c) . sum . map demF) sol :: [Double]
		dist = map (\x -> sum . map distF $ zip x (tail x)) sol0
		sol0 = map ((0 :) . (++ [0])) sol

solve :: DistF -> DemF -> Capacity -> State -> (Score, [Path])
solve distF demF c s = (score distF demF c solution, solution)
	where solution = splitOn [0] $ M.elems s

neighbour :: (State -> (Score, [Path])) -> (Size, Size) -> Solution -> Rand StdGen Solution
neighbour solver rang (_, (s, _)) = do
	a <- getRandomR rang
	b <- getRandomR rang
	let nSt = switch a b s
	let (nSc, sol) = solver nSt
	return (nSc, (nSt, sol))

switch :: Vertex -> Vertex -> State -> State
switch a b m = M.insert b (m M.! a) $ M.insert a (m M.! b) m

allNeigh :: (State -> (Score, [Path])) -> Solution -> [Solution]
allNeigh solver (_, (s, _)) = map ((\ w -> scc (solver w) w) . (\ (x, y) -> switch x y s)) pairs
	where 	scc (e, sol) st = (e, (st, sol))
		pairs = [(x, y) | x <- [1..M.size s], y <- [1..M.size s], x < y]
