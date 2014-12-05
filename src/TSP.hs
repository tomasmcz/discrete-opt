module TSP 
  ( -- * Types
    Size
  , Vertex
  , Path
  , Distance
  , Coordinate
    -- * Array types
  , CArray
  , DArray
  , FDist
    -- * Data manipulation
  , getDistances
  , distF2dist
  , readMatrix
  , distC
  , storeCoords
  ) where

import Data.Array.Unboxed

type Vertex = Int
type Path = [Vertex]
type Size = Int
type Distance = Double
type Coordinate = Double
type DArray = UArray (Vertex, Vertex) Distance
type CArray = UArray Vertex Coordinate
type FDist = ((Vertex, Vertex) -> Distance)


storeCoords :: Size -> String -> (CArray, CArray)
storeCoords n s = (listArray (1, n) $ map fst lst, listArray (1, n) $ map snd lst)
	where	lst = take n . map (lst2 . map read . words) . lines $ s
		lst2 [a, b] = (a, b)
                lst2 _ = error "error while reading coordinates"
--	where	readList = unfoldr (readDouble . B.dropWhile isSpace) :: B.ByteString -> [Distance]

distC :: (CArray, CArray) -> (Vertex, Vertex) -> Distance
distC (s, d) (a, b) = euc2 (s ! a) (d ! a) (s ! b) (d ! b)

euc2 :: Coordinate -> Coordinate -> Coordinate -> Coordinate -> Distance
euc2 x1 y1 x2 y2 = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

rList :: String -> [Distance]
rList = map read . words

getDistances :: Size -> String -> DArray
getDistances n source = listArray ((1, 1), (n, n)) $ take (n * n) (rList source)

readMatrix :: Int -> String -> DArray
readMatrix n = listArray ((1,1), (n,n)) . map read . words 

distF2dist :: Int -> FDist -> DArray
distF2dist n distf = listArray ((1,1), (n, n)) . map distf $ [(a, b) | a <- [1..n], b <- [1..n]]
