module Main.ACO where

import System.Console.GetOpt

import TSP
import ACO
import Main.Opts

mainACO :: [Flag] -> [String] -> IO ()
mainACO o ("matrix":file:_) = runACO o $ MatrixFile file
mainACO o ("coords":file:_) = runACO o $ Euc2DFile file
mainACO _ _ = putStrLn $ usageInfo header options

runACO :: [Flag] -> TSPFile -> IO ()
runACO opts file = do
  let conf n = foldl modConfig (defConfig n) opts
  minPath <- (\ (n, p) -> optimize (conf n) p) =<< readProblemMatrix file 
  putStrLn $ show (fst minPath) ++ " 0"
  mapM_ (putStr . (++ " ") . show . (+ (-1))) . tail $ snd minPath
  putStrLn ""


modConfig :: ConfigACO -> Flag -> ConfigACO
modConfig conf (FGen g) = conf {paramNGen = g}
modConfig conf (FAnts a) = conf {paramAGen = a}
modConfig conf FTwoOpt = conf {paramUse2Opt = True}
modConfig conf _ = conf
