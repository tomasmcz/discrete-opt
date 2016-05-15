{-# LANGUAGE ViewPatterns #-}

module Main.TSP where

import Control.Monad
import Control.Monad.Random
import System.Console.GetOpt

import ACO
import SA
import TSP
import TSP.NN
import qualified TSP.TwoOpt

import Main.Opts
import Main.Plot

getTSPfile :: [Flag] -> [String] -> TSPFile
getTSPfile ((FCoords `elem`) -> True) (file:_) = Euc2DFile file
getTSPfile _ (file:_) = MatrixFile file
getTSPfile _ _ = error $ usageInfo header options

saConfig :: SA.Config
saConfig = SA.Config 300000 0.99 10 7 10

printResult :: Size -> Path -> IO ()
printResult n minPath = do
  print (fst minPath)
  mapM_ (putStr . (++ " ") . show . subtract 1) . take n $ snd minPath
  putStrLn ""

tspSA :: [Flag] -> [String] -> IO ()
tspSA opts args = do
  (n, distf) <- readProblemFunction $ getTSPfile opts args
  let conf = saConfig
  (minPath1, inf) <- evalRandIO $ SA.optimize
                     conf
                     (neighbours distf n)
                     (score distf $ [1..n] ++ [1], [1..n] ++ [1]) -- TODO random initial state
  when (FVerb `elem` opts) $ mapM_ putStrLn inf
  case fPlot opts of
    Nothing -> return ()
    Just file -> plotScore file (info conf) $ map ((read :: String -> Double) . head . words) inf
  let minPath = if FTwoOpt `elem` opts 
        then let twoO = TSP.TwoOpt.optimize distf $ snd minPath1 in
                          (sum $ zipWith (curry distf) twoO (tail twoO), twoO)
        else minPath1
  printResult n minPath

tspNN :: [Flag] -> [String] -> IO ()
tspNN opts args = do
  (n, dist) <- readProblemFunction $ getTSPfile opts args
  let path = TSP.NN.optimize dist n
  let minPath = if FTwoOpt `elem` opts
        then let twoO = TSP.TwoOpt.optimize dist $ snd path in
                          (sum $ zipWith (curry dist) twoO (tail twoO), twoO)
        else path
  printResult n minPath

tspACO :: [Flag] -> [String] -> IO ()
tspACO opts args = do
  let conf n = foldl modConfig (defConfig n) opts
  (minPath, inf) <- (\ (n, p) -> ACO.optimizeWithInfo (conf n) p) =<< readProblemMatrix (getTSPfile opts args) 
  when (FVerb `elem` opts) $ mapM_ print inf
  case fPlot opts of
    Nothing -> return ()
    Just file -> plotScore file 1 inf
  printResult n minPath
