module Main.ACO where

import TSP
import ACO

mainACO :: a -> FilePath -> IO ()
mainACO _ file = do
  minPath <- readProblemMatrix file >>= uncurry (optimize defConfig)
  putStrLn $ show (fst minPath) ++ " 0"
  mapM_ (putStr . (++ " ") . show . (+ (-1))) . tail $ snd minPath
  putStrLn ""
