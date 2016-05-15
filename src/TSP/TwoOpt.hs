{- |
Copyright    : 2014 Tomáš Musil
License      : BSD-3
Stability    : experimental
Portability  : portable

This module implements the 2-opt heuristic for TSP.

-}

module TSP.TwoOpt (optimize) where

import TSP

type Pair = Int

-- TODO: learn quick-check and get this to work
--pairs :: Path -> [(Vertex, Vertex, Vertex, Vertex)]
--pairs path = [(a, b, c, d) | (a, b) <- p, (c, d) <- p, a /= d, b /= c, a < c]
--    where p = zip path (tail path)

pairs (l:ls:lss) = pairs' l ls lss (ls:lss)

pairs' p r (l:ls:lss) list = (p, r, l, ls) : pairs' p r (ls:lss) list
pairs' _ _ _ (k:ks:kss) = pairs' k ks kss (ks:kss)
pairs' _ _ _ _ = []

dist' dist (a, b, c, d) = (dist (a, b) + dist (c, d) - (dist (a, c) + dist (b, d)), a, c)

optimize :: FDist -> Path -> Path
optimize dist list | n > 0 = optimize dist $ makeChange a c [] list
                   | n <= 0 = list
    where (n, a, c) = bestPair dist list

bestPair :: FDist -> Path -> (Distance, Pair, Pair)
bestPair dist list = maximum $ map (dist' dist) (pairs list)

makeChange a c [] (l:ls)
    | l /= a = l : makeChange a c [] ls
    | l == a = l : makeChange a c [head ls] (tail ls)
makeChange a c acc (l:ls)
    | l /= c = makeChange a c (l:acc) ls
    | l == c = (l:acc) ++ ls
