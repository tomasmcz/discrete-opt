module Main.VRP where

import Control.Monad
import Control.Monad.Random
import System.Random ()
import qualified Data.Map as M

import ACO
import SA
import Problems.VRP
import Main.Opts
import Main.Plot

saConfig :: SA.Config
saConfig = SA.Config 300000 0.99 100 7 100

vrpSA :: [Flag] -> [String] -> IO ()
vrpSA opts (file:_) = do
  (n, v, c, distF, demF) <- readProblemF file
  let initState = M.fromList . zip [1..] $ [1..n-1] ++ replicate (v - 1) 0
  let (initSc, initSol) = solve distF demF c initState
      conf = foldl modConfigSA saConfig opts
  (solut, info1) <- evalRandIO $ SA.optimize conf (neighbour (solve distF demF c) (1, M.size initState)) (initSc, (initState, initSol))
  let (solution, info2) = SA.descent (allNeigh (solve distF demF c)) solut
      allInfo = map (read . head . words) info1 ++ map read info2 
  when (FVerb `elem` opts) $ mapM_ print allInfo
  case fPlot opts of
    Nothing -> return ()
    Just f -> plotScore f (info conf) allInfo
  print (fst solution)
  mapM_ (putStrLn . unwords . map show . (0 :) . (++ [0])) (snd . snd $ solution)

vrpACO :: [Flag] -> [String] -> IO ()
vrpACO opts (file:_) = do
  (n, v, dist, p) <- readProblemM file
  let conf = (foldl modConfigACO (defConfig (n - 1)) opts) {penalty = p, originRandom = False, returnToOrigin = v - 1}
  (minPath, inf) <- ACO.optimizeWithInfo conf dist
  when (FVerb `elem` opts) $ mapM_ print inf
  case fPlot opts of
    Nothing -> return ()
    Just f -> plotScore f 1 inf
  print (fst minPath) 
  mapM_ (putStr . (++ " ") . show) $ snd minPath
  putStrLn ""
