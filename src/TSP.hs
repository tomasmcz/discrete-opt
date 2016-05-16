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
    -- * Configuration
  , Config(..)
  , TSPFile(..)
  , DistConf(..)
  , defConfig
    -- * Data manipulation
  , readProblemMatrix
  , readProblemFunction
    -- * Problem specification
  , neighbours
  , score
  ) where

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
type DistanceComp = Coordinate -> Coordinate -> Coordinate -> Coordinate -> Distance

data TSPFile = MatrixFile FilePath | Euc2DFile FilePath
data DistConf = Euc2D | Euc2DRounded

data Config = Config
  { tspfile :: TSPFile
  , distanceC :: DistConf
  }

defConfig :: TSPFile -> Config
defConfig f = Config f Euc2D

readProblemMatrix :: Config -> IO (Size, DArray)
readProblemMatrix conf = case tspfile conf of
  (Euc2DFile _) -> (\(n, distf) -> (n, distF2dist n distf)) <$> readProblemFunction conf
  (MatrixFile filePath) -> withFile filePath ReadMode $ \ file -> do
    n <- read <$> hGetLine file
    dist <- readMatrix n <$> hGetContents file
    dist ! (n, n) `seq` return (n, dist)
    

readProblemFunction :: Config -> IO (Size, FDist)
readProblemFunction conf = case tspfile conf of
  (Euc2DFile filePath) -> withFile filePath ReadMode $ \ file -> do
    n <- read <$> hGetLine file
    distf <- distC (fsel $ distanceC conf) . storeCoords n <$> hGetContents file
    distf (n, n) `seq` return (n, distf)
  (MatrixFile _) -> (\(n, dist) -> (n, (dist !))) <$> readProblemMatrix conf

fsel :: DistConf -> DistanceComp
fsel Euc2D = euc2
fsel Euc2DRounded =rEuc2

storeCoords :: Size -> String -> (CArray, CArray)
storeCoords n s = (listArray (1, n) $ map fst lst, listArray (1, n) $ map snd lst)
  where
    lst = take n . map (lst2 . map read . words) . lines $ s
    lst2 [a, b] = (a, b)
    lst2 _ = error "error while reading coordinates"

distC :: DistanceComp -> (CArray, CArray) -> (Vertex, Vertex) -> Distance
distC dc (s, d) (a, b) = dc (s ! a) (d ! a) (s ! b) (d ! b)

euc2 :: DistanceComp
euc2 x1 y1 x2 y2 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

rEuc2 :: DistanceComp
rEuc2 x1 y1 x2 y2 = fromIntegral . round $ sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

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
