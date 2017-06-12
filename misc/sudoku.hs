#!/usr/bin/env stack
{- stack
    --resolver lts-7.1
    --install-ghc
    runghc
-}

import Data.List (sort)

data Var = Var Int Int Int
data Lit = PLit Var
         | NLit Var
type Clause = [Lit]

showVar :: Var -> String
showVar (Var a b c) = show a ++ show b ++ show c

showLit :: Lit -> String
showLit (PLit v) = showVar v
showLit (NLit v) = "-" ++ showVar v

showClause :: Clause -> String
showClause = unwords . (++ ["0"]) . map showLit

atLeastOneInEntry :: [Clause]
atLeastOneInEntry = [aloie x y | x <- [1..9], y <- [1..9]]
  where aloie a b = [PLit (Var f a b) | f <- [1..9]]

atMostOne :: [Var] -> [Clause]
atMostOne (a:as) = [[NLit a, NLit b] | b <- as] ++ atMostOne as
atMostOne [] = []

atMostOneInEntry :: [Clause]
atMostOneInEntry = concat [atMostOne [Var a b c | a <- [1..9]] | b <- [1..9], c <- [1..9]]

atLeastOneInRow :: [Clause]
atLeastOneInRow = [[PLit (Var a b c) | b <- [1..9]] | a <-[1..9], c <- [1..9]]

atLeastOneInColumn :: [Clause]
atLeastOneInColumn = [[PLit (Var a b c) | c <- [1..9]] | a <-[1..9], b <- [1..9]]

atLeastOneInSquare :: [Clause]
atLeastOneInSquare = [[PLit (Var a b c) | (b, c) <- sq] | a <- [1..9], sq <- squares]

squares = [[(a, b) | a <- [1..3], b <- [1..3]]
           ,[(a, b) | a <- [4..6], b <- [1..3]]
           ,[(a, b) | a <- [7..9], b <- [1..3]]
           ,[(a, b) | a <- [1..3], b <- [4..6]]
           ,[(a, b) | a <- [4..6], b <- [4..6]]
           ,[(a, b) | a <- [7..9], b <- [4..6]]
           ,[(a, b) | a <- [1..3], b <- [7..9]]
           ,[(a, b) | a <- [4..6], b <- [7..9]]
           ,[(a, b) | a <- [7..9], b <- [7..9]]]

atMostOneInRow :: [Clause]
atMostOneInRow = concat [atMostOne [Var a b c | b <- [1..9]] | a <- [1..9], c <- [1..9]]

atMostOneInColumn :: [Clause]
atMostOneInColumn = concat [atMostOne [Var a b c | c <- [1..9]] | a <- [1..9], b <- [1..9]]

atMostOneInSquare :: [Clause]
atMostOneInSquare = concat [atMostOne [Var a b c | (b, c) <- sq] | a <- [1..9], sq <- squares]

minCoding :: [Clause]
minCoding = atLeastOneInEntry
              ++ atMostOneInRow
              ++ atMostOneInColumn
              ++ atMostOneInSquare

maxCoding :: [Clause]
maxCoding = atMostOneInEntry
              ++ atLeastOneInRow
              ++ atLeastOneInColumn
              ++ atLeastOneInSquare

printSudoku :: String -> IO ()
printSudoku s = printSudoku' s 9 10

printSudoku' :: String -> Int -> Int -> IO ()
printSudoku' [] _ _ = putStrLn ""
printSudoku' (s:ss) a 0 = do
  putStrLn ""
  printSudoku' (s:ss) (a - 1) 10
printSudoku' (s:ss) a 10 | a == 6 || a == 3 = do
  putStrLn "---------------------"
  printSudoku' (s:ss) a 9
printSudoku' (s:ss) a 10 =  printSudoku' (s:ss) a 9
printSudoku' (s:ss) a b | b == 7 || b == 4 = do
  putStr $ s : ' ' : '|' : " "
  printSudoku' ss a (b - 1)
printSudoku' (s:ss) a b = do
  putStr $ s : " "
  printSudoku' ss a (b - 1)

showDIMACS :: Int -> [Clause] -> [String]
showDIMACS nVars cs = ("p " ++ show nVars ++ " " ++ show (length cs)) : map showClause cs  

cnfInput :: String -> [Clause]
cnfInput s = [[PLit (Var (read [a]) b c)] | (a, (b, c)) <- zip s [(x, y) | x <-[1..9], y <- [1..9]], a /= '.']

readSolution :: String -> String
readSolution = phrase2 . phrase1 . words . phrase0 . lines
  where phrase1 (('-':_):ws) = phrase1 ws
        phrase1 ("v":ws) = phrase1 ws
        phrase1 ("0":ws) = []
        phrase1 ([a,b,c]:ws) = (read [b] :: Int, read [c] :: Int, read [a] :: Int) : phrase1 ws
        phrase2 ws = concatMap (show . (\(_, _, a) -> a)) $ sort ws
        phrase0 (l@('v':' ':_):_) = l
        phrase0 (_:ls) = phrase0 ls

main :: IO ()
main = do
  --problems <- (take 10000 . lines) <$> readFile "sudoku-inputs.txt"
  --let pn = 5999
  --let p1 = problems !! (pn - 1)
  --printSudoku p1
  --putStrLn ""
  --putStrLn . unlines . showDIMACS 999 $ cnfInput p1 ++ minCoding
  --mapM_ (uncurry writeFile) . zip (map ((++ ".cnf") . ("maxcoding/" ++) . show) [1..]) $ map (\p -> unlines . showDIMACS 999 $ cnfInput p ++ maxCoding) problems
  --mapM_ (uncurry writeFile) . zip (map ((++ ".cnf") . ("mincoding/" ++) . show) [1..]) $ map (\p -> unlines . showDIMACS 999 $ cnfInput p ++ minCoding) problems
  --solution <- readFile $ "maxcoding/" ++ show pn ++ ".sol"
  --printSudoku $ readSolution solution
  --putStrLn ""
  --solution <- readFile $ "mincoding/" ++ show pn ++ ".sol"
  --printSudoku $ readSolution solution
  putStrLn "maxcoding"
  times <- (map (read :: String -> Double) . words) <$> readFile "maxtimes"
  putStrLn "avg:"
  print $ sum times / 10000.0
  putStrLn "min:"
  print $ minimum times
  putStrLn "max:"
  print $ maximum times
  putStrLn "mincoding"
  times <- (map (read :: String -> Double) . words) <$> readFile "mintimes"
  putStrLn "avg:"
  print $ sum times / 10000.0
  putStrLn "min:"
  print $ minimum times
  putStrLn "max:"
  print $ maximum times
