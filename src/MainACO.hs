module MainACO where

import System.IO

import TSP
import ACO

mainACO :: a -> FilePath -> IO ()
mainACO _ file = do
  nLine <- getLine
  let n = read nLine :: Size
  --dLines <- B.getContents
  dLines <- getContents
  let dist = distF2dist n $ distC (storeCoords n dLines)
  --let dist = readMatrix n dLines
  minPath <- optimize n dist
  putStrLn $ show (fst minPath) ++ " 0"
  mapM_ (putStr . (++ " ") . show . (+ (-1))) . take n $ snd minPath
  putStrLn ""
