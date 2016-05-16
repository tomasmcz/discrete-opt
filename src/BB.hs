{- |
Copyright    : 2014 Tomáš Musil
License      : BSD-3
Stability    : experimental
Portability  : portable

Branch and bound solver for TSP.

-}

module BB
  (
  -- * Configuration
    Config(..)
  , defConfig
  -- * Optimization
  , optimize
  --, optimizeWithInfo
  ) where

import TSP

import Control.Monad.LPMonad
import Data.LinearProgram
import Data.Array.Unboxed
import Data.Tuple
import Data.Graph hiding (edges, Vertex)
import Data.Tree
import Data.List
import qualified Data.Map as M
import System.IO

data Config = Config 
  { paramSize :: Size          -- ^ size of the graph
  , initEst :: Maybe Double    -- ^ initial estimate
  }

-- | Default configuration.
defConfig :: Size -> Config
defConfig n = Config n Nothing

objFun :: Size -> FDist ->  LinFunc String Double
objFun n dist = linCombination [(dist (a, b), "e_" ++ show a ++ "_" ++ show b) | a <- [1..n], b <- [a+1..n]]

lp :: Size -> FDist -> ([(LinFunc String Double, Double)], [(String, Double)]) -> LP String Double
lp n dist (addit, branch) = execLPM $ do
  setDirection Min
  setObjective (objFun n dist)
  mapM_ (\ lst -> equalTo (varSum lst) 2) [[vv2str a b | b <- [1..n], a /= b] | a <- [1..n]]
  mapM_ (\ s -> varBds s 0 1) ["e_" ++ show a ++ "_" ++ show b | a <- [1..n], b <- [a+1..n]]
  mapM_ (uncurry geqTo) addit
  mapM_ (uncurry varEq) branch

str2vv :: String -> (Vertex, Vertex)
str2vv s = (p, d)
  where st = drop 2 s
        p = read $ takeWhile (/= '_') st :: Int
        d = read $ drop 1 $ dropWhile (/= '_') st :: Int

vv2str :: Vertex -> Vertex -> String
vv2str a b = "e_" ++ show (min a b) ++ "_" ++ show (max a b)

-- ? getEdges :: Maybe (_, a) -> [String]
getEdges (Just (_, e)) = M.toList . M.filter (> 0.00001) $ e

getCycles n edges = if bigCycle cycles then [] else map cond (cycles ++ islandPaths) ++
  (if null cycles && null islandPaths then map combCond combs else [])
    where
      lst = map (str2vv . fst) green
      lstRed = map (str2vv . fst) red
      (green, red) = partition ((> 0.99999) . snd) edges
      cmps = map flatten $ components greenGraph
      redCmps = map flatten $ components redGraph
      greenGraph = buildG (1, n) (lst ++ map swap lst)
      redGraph = buildG (1, n) (lstRed ++ map swap lstRed)
      dg = indegree greenGraph
      (cycles, pathsAndPoints) = partition (all (== 2) . map (dg !)) cmps
      paths = filter isNotLOne pathsAndPoints
      redNames = map fst red
      islandPaths = [p | p <- paths, uncurry vv2str (ends p) `elem` redNames]
      endsToT [a, b] = (a, b)
      ends = endsToT . filter ((== 1) . (dg !))
      cond l = (varSum [vv2str a b | a <- l, b <- [1..n], a /= b, b `notElem` l], 2)
      combCondOne l =  [vv2str a b | a <- l, b <- [1..n], a /= b, b `notElem` l]
      combCond l = ( linCombination . map (\ l -> (fromIntegral $ length l :: Double, head l)) .
                       group . sort . concatMap combCondOne $ l
                   , fromIntegral ((length l - 1) * 3 + 1) :: Double)
      bigCycle [a] = length a == n
      bigCycle _ = False
      isNotLOne [_] = False
      isNotLOne _ = True
      comb rc = rc : takeMaxOdd (tooths rc)
      tooths rc = [ [p, otherEnd p] | p <- rc, (dg ! p) > 0, otherEnd p `notElem` rc
                  , all (\ x -> (x == p) || x `notElem` rc)  (greenGraph ! otherEnd p)]
      otherEnd p = head $ greenGraph ! p
      combs = filter ((>= 4) . length) . map comb $ redCmps
       

takeMaxOdd :: [a] -> [a]
takeMaxOdd (l:ls) = l : tMO ls
  where tMO (s:ss:sss) = s : ss : tMO sss
        tMO _ = []

optAll solver cycler conds branch = do
  (_, solution) <- solver (conds, branch)
  case solution of
    Nothing -> return (Nothing, [])
    Just _ -> do
      let cycles = filter (`notElem` conds) $ cycler solution
      if null cycles
        then return (solution, conds)
        else optAll solver cycler (conds ++ cycles) branch

optBB estimator conds branch best c p1 p2 = do
  (solution, cycles) <- estimator conds branch
  case solution of
    Nothing -> return (Nothing, c)
    Just _ ->
      if solVal solution > best + 0.001
        then do
          hPutStrLn stderr $ "CURRENT BEST: " ++ show best
          hPutStrLn stderr $ "ESTIMATED COMPLETION: " ++ show ((p1 + p2) * 100)
          return (Nothing, c)
        else do
          let edges = filter ((< 0.99999) . snd) . getEdges $ solution
          if null edges
            then do
              hPutStrLn stderr ""
              hPutStrLn stderr $ "NEW BEST: " ++ show (min best $ solVal solution)
              hPutStrLn stderr $ "ESTIMATED COMPLETION: " ++ show ((p1 + p2) * 100)
              hPutStrLn stderr ""
              return (solution, c)
            else do
              let edge = fst . minimumBy (\x y -> compare (abs(0.5 - snd x)) (abs (0.5 - snd y))) $ edges
              (sol1, d) <- optBB estimator cycles ((edge, 0.0):branch) best (c + 1) p1 (p2 / 2)
              (sol2, e) <- optBB estimator cycles ((edge, 1.0):branch) (min best $ solVal sol1) d (p1 + p2 / 2) (p2 / 2)
              return (if solVal sol1 < solVal sol2 then sol1 else sol2, e)

solVal (Just (v, _)) = v
solVal _ = 9999999999999

optimize :: FDist -> Config -> IO ()
optimize dist conf = do
  {- best <- if length args == 1
    then return (read . head $ args :: Distance)
    else do
      ants <- Ants.optimize n undefined
      hPutStrLn stderr $ "ANTS: " ++ show (fst ants)
      return $ fst ants
  -}
  let (Just best) = initEst conf
  --let solver = glpSolveVars simplexDefaults . lp n dist
      n = paramSize conf
      solver = glpSolveVars (SimplexOpts MsgOff 60 True) . lp n dist
      cycler = getCycles n . getEdges
  (solution, count)  <- optBB (optAll solver cycler) [] [] best 0 0.0 1.0
  hPutStrLn stderr $ "BRANCH COUNT: " ++ show count
  putStrLn ""
  putStrLn "--EDGES"
  mapM_ (putStrLn . drop 2 . (\ (a,b) -> a ++ " " ++ show b)) . getEdges $ solution