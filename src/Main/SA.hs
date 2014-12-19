module Main.SA where

import Control.Monad.Random
import System.Random ()

import SA
import TSP
import qualified TSP.TwoOpt

neighbours :: FDist -> Size -> (Score, Path) -> Rand StdGen (Score, Path)
neighbours distf n (_, st) = do
  a <- getRandomR (1, n) 
  c <- getRandomR (1, n) 
  let state = makeChange (min a c) (max a c) st
  return (score distf state, state)


score distf s = sum $ zipWith (curry distf) s (tail s)

-- dist' dist (a, b, c, d) = (dist (a, b) + dist (c, d) - (dist (a, c) + dist (b, d)), a, c)

makeChange a c path = take a path ++ reverse (take (c - a) (drop a path)) ++ drop c path

saConfig :: SA.Config
saConfig = SA.Config 300000 0.99 100 7 10

mainSA :: a -> b -> IO ()
mainSA _ _ = do
	nLine <- getLine
	let n = read nLine :: Size
	--dLines <- B.getContents
	dLines <- getContents
	let distf = distC (storeCoords n dLines)
	(minPath1, _) <- evalRandIO $ SA.optimize
                      saConfig
                      (neighbours distf n)
                      (score distf $ [1..n] ++ [1], [1..n] ++ [1]) -- TODO random initial state
	let twoO = TSP.TwoOpt.optimize distf $ snd minPath1
	let minPath = (sum $ zipWith (curry distf) twoO (tail twoO), twoO)
	putStrLn $ show (fst minPath) ++ " 0"
	mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd minPath
        putStrLn ""
