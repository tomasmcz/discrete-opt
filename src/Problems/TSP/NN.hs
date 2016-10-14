{- |
Copyright    : 2014 Tomáš Musil
License      : BSD-3
Stability    : experimental
Portability  : portable

Nearest Neighbour heuristic for TSP.

-}

module Problems.TSP.NN
 ( optimize
 ) where

import Control.Arrow
--import Data.List.Stream
--import Prelude hiding ((++), lines, map, minimum, splitAt, sum, repeat, tail, take, words, zip)
--TODO zrušit explicitní rekurzi a zkusit znovu
import qualified Data.Set as Set

import Problems.TSP
import qualified Problems.TSP.TwoOpt as Topt

findPath :: Size -> FDist -> Vertex -> Path
findPath n dist origin = origin : fp dist origin origin (Set.delete origin (Set.fromAscList [1..n]))

fp dist frst lst unv | not (Set.null unv) = nearest : fp dist frst nearest (Set.delete nearest unv)
    where nearest = snd (minimum (map dist' (Set.elems unv)))
          dist' x = (dist (lst, x), x)
fp _ frst _ _ = [frst]

allPaths :: FDist -> Size -> [Path]
--allPaths dist n = map (findPath n dist) [1..n]
allPaths dist n = map (Topt.optimize dist . findPath n dist) [1..n]

pathLen :: FDist -> Path -> Distance
pathLen dist path = sum . map dist $ zip path (tail path)

optimize :: FDist -> Int -> (Distance, Path)
optimize dist = minimum . allPathlens dist

allPathlens :: FDist -> Int -> [(Distance, Path)]
allPathlens dist n = map (pathLen dist &&& id) $ allPaths dist n
