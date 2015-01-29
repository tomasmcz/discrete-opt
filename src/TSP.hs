{- |
Copyright    : 2014 Tomáš Musil
License      : BSD-3
Stability    : experimental
Portability  : portable 

Traveling Salesman Problem.

-}

module TSP 
  ( -- * Types
    Size
  , Vertex
  , Path
  , Distance
  , Coordinate
  , Score
    -- * Array types
  , CArray
  , DArray
  , FDist
    -- * Data manipulation
  , TSPFile(..)
  , readProblemMatrix
  , readProblemFunction
    -- * Problem specification
  , neighbours
  , score
  ) where

import Control.Applicative
import Control.Monad.Random
import Data.Array.Unboxed
import System.IO

type Vertex = Int
type Path = [Vertex]
type Size = Int
type Distance = Double
type Coordinate = Double
type DArray = UArray (Vertex, Vertex) Distance
type CArray = UArray Vertex Coordinate
type FDist = ((Vertex, Vertex) -> Distance)
type Score = Double

data TSPFile = MatrixFile FilePath | Euc2DFile FilePath

readProblemMatrix :: TSPFile -> IO (Size, DArray)
readProblemMatrix f@(Euc2DFile _) = (\(n, distf) -> (n, distF2dist n distf)) <$> readProblemFunction f
readProblemMatrix (MatrixFile filePath) = withFile filePath ReadMode $ \ file -> do
  n <- read <$> hGetLine file
  dist <- readMatrix n <$> hGetContents file
  dist ! (n, n) `seq` return (n, dist)
    

readProblemFunction :: TSPFile -> IO (Size, FDist)
readProblemFunction (Euc2DFile filePath) = withFile filePath ReadMode $ \ file -> do
  n <- read <$> hGetLine file
  distf <- distC . storeCoords n <$> hGetContents file
  distf (n, n) `seq` return (n, distf)
readProblemFunction f@(MatrixFile _) = (\(n, dist) -> (n, (dist !))) <$> readProblemMatrix f

storeCoords :: Size -> String -> (CArray, CArray)
storeCoords n s = (listArray (1, n) $ map fst lst, listArray (1, n) $ map snd lst)
	where	lst = take n . map (lst2 . map read . words) . lines $ s
		lst2 [a, b] = (a, b)
                lst2 _ = error "error while reading coordinates"

distC :: (CArray, CArray) -> (Vertex, Vertex) -> Distance
distC (s, d) (a, b) = euc2 (s ! a) (d ! a) (s ! b) (d ! b)

euc2 :: Coordinate -> Coordinate -> Coordinate -> Coordinate -> Distance
euc2 x1 y1 x2 y2 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

readMatrix :: Int -> String -> DArray
readMatrix n = listArray ((1,1), (n,n)) . map read . words 

distF2dist :: Int -> FDist -> DArray
distF2dist n distf = listArray ((1,1), (n, n)) . map distf $ [(a, b) | a <- [1..n], b <- [1..n]]

neighbours :: FDist -> Size -> (Score, Path) -> Rand StdGen (Score, Path)
neighbours distf n (_, st) = do
  a <- getRandomR (1, n) 
  c <- getRandomR (1, n) 
  let state = makeChange (min a c) (max a c) st
  --    newScore = (dist (a, b) + dist (c, d) - (dist (a, c) + dist (b, d)), a, c)
  return (score distf state, state)

score :: FDist -> Path -> Score
score distf s = sum $ zipWith (curry distf) s (tail s)

makeChange :: Int -> Int -> Path -> Path
makeChange a c path = take a path ++ reverse (take (c - a) (drop a path)) ++ drop c path


