module Main.ACO where

import System.Console.GetOpt

import TSP
import ACO
import Main.Opts

mainACO :: [Flag] -> [String] -> IO ()
mainACO _ ("matrix":file:_) = runACO $ MatrixFile file
mainACO _ ("coords":file:_) = runACO $ Euc2DFile file
mainACO _ _ = putStrLn $ usageInfo header options

runACO :: TSPFile -> IO ()
runACO file = do
  minPath <- uncurry (optimize defConfig) =<< readProblemMatrix file 
  putStrLn $ show (fst minPath) ++ " 0"
  mapM_ (putStr . (++ " ") . show . (+ (-1))) . tail $ snd minPath
  putStrLn ""
