{-# LANGUAGE ViewPatterns #-}

module Main.TSP where

import Control.Monad.Random
import System.Console.GetOpt

import ACO
import SA
import TSP
import TSP.NN
import qualified TSP.TwoOpt
import Main.Opts

getTSPfile :: [Flag] -> [String] -> TSPFile
getTSPfile ((FCoords `elem`) -> True) (file:_) = Euc2DFile file
getTSPfile _ (file:_) = MatrixFile file
getTSPfile _ _ = error $ usageInfo header options

saConfig :: SA.Config
saConfig = SA.Config 300000 0.99 100 7 10

tspSA :: [Flag] -> [String] -> IO ()
tspSA opts args = do
  (n, distf) <- readProblemFunction $ getTSPfile opts args
  (minPath1, _) <- evalRandIO $ SA.optimize
                     saConfig
                     (neighbours distf n)
                     (score distf $ [1..n] ++ [1], [1..n] ++ [1]) -- TODO random initial state
  let twoO = TSP.TwoOpt.optimize distf $ snd minPath1
  let minPath = (sum $ zipWith (curry distf) twoO (tail twoO), twoO)
  putStrLn $ show (fst minPath) ++ " 0"
  mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd minPath
  putStrLn ""

tspNN :: [Flag] -> [String] -> IO ()
tspNN opts args = do
  (n, dist) <- readProblemFunction $ getTSPfile opts args
  let path = TSP.NN.optimize dist n
--  let paths = map (findPath n dist) [1..n]
--  let path = minimum $ map (pathLen dist &&& id) paths
  putStrLn $ show (fst path) ++ " 0"
--  mapM_ (putStr . (++ " ") . show) . take n $ snd path
  mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd path
  putStrLn ""

tspACO :: [Flag] -> [String] -> IO ()
tspACO opts args = do
  let conf n = foldl modConfig (defConfig n) opts
  minPath <- (\ (n, p) -> ACO.optimize (conf n) p) =<< readProblemMatrix (getTSPfile opts args) 
  putStrLn $ show (fst minPath) ++ " 0"
  mapM_ (putStr . (++ " ") . show . (+ (-1))) . tail $ snd minPath
  putStrLn ""

modConfig :: ConfigACO -> Flag -> ConfigACO
modConfig conf (FGen g) = conf {paramNGen = g}
modConfig conf (FAnts a) = conf {paramAGen = a}
modConfig conf FTwoOpt = conf {paramUse2Opt = True}
modConfig conf _ = conf
