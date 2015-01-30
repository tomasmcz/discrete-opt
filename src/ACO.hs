{- |
Copyright    : 2014 Tomáš Musil
License      : BSD-3
Stability    : experimental
Portability  : portable 

Ant Colony Optimization.

-}

module ACO 
  (
  -- * Types
    Pheromon
  , Coefficient
  -- * Configuration
  , ConfigACO(..)
  , defConfig
  -- * Optimization
  , optimize
  , optimizeWithInfo
  ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Array.Unboxed
import qualified Data.IntSet as Set
import Data.List (foldl')
--import Data.List.Stream
--import Prelude hiding (map, minimum, splitAt, sum, repeat, tail, take, zip)

import TSP hiding (CArray)
import qualified TSP.TwoOpt as Topt

type Probability = Double
type Pheromon = Double
type Coefficient = Double
type PArray = UArray (Vertex, Vertex) Pheromon
type CArray = UArray (Vertex, Vertex) Coefficient
type FPher = ((Vertex, Vertex) -> Pheromon)
type FCoef = ((Vertex, Vertex) -> Coefficient)

data ConfigACO = ConfigACO 
  { paramSize :: Size          -- ^ size of the graph
  , paramAGen :: Int           -- ^ number of ants in one generation
  , paramNGen :: Int           -- ^ number of generations
  , paramInitPh :: Pheromon    -- ^ initial value of pheromones
  , paramAlpha  :: Coefficient -- ^ Alpha parameter
  , paramBeta   :: Coefficient -- ^ Beta parameter
  , paramEvRate :: Pheromon    -- ^ evaporation rate
  , paramPBest  :: Pheromon    -- ^ PBest mmas parameter
  , paramUse2Opt :: Bool       -- ^ use 2-Opt heuristic
  , penalty :: Path -> Double -> Double  -- ^ penalty function
  , originRandom :: Bool       -- ^ randomize origin
  , returnToOrigin :: Int      -- ^ return to origin n times
  }

-- TODO better type for penalty
-- TODO remove all the HACKs

type ACOm a = ReaderT ConfigACO (Rand StdGen) a

-- | Default configuration.
defConfig :: Size -> ConfigACO
defConfig n = ConfigACO n
                      32 
                      500
                      1e16
                      1.0 
                      2.0 
                      0.001
                      0.0000005 
                      False
                      (const id)
                      True
                      0

newPhrArray :: (Monad m) => ReaderT ConfigACO m PArray
newPhrArray = do
  n <- asks paramSize
  initPh <- asks paramInitPh
  r <- asks returnToOrigin 
  let b = if r > 0 then 0 else 1 -- HACK
  return . listArray ((b, b), (n, n)) $ repeat initPh

updatePheromones :: (Monad m) => Pheromon -> Distance -> (Distance, Path) -> PArray -> ReaderT ConfigACO m PArray
updatePheromones pBestCoef minP (ln, bp) phrmn = do
  evRate <- asks paramEvRate
  let evap = amap ((1.0 - evRate) *)

      update = flip (accum (+)) pathPairs 
      pathPairs = zip (zip bp (tail bp)) (repeat addC)
      addC = 10.0 / ln :: Pheromon

      bound = amap (min phMax . max phMin)
      phMax = 10.0 / (evRate * minP) :: Pheromon
      phMin = phMax * pBestCoef
  return . bound . update $ evap phrmn

coefs :: ConfigACO -> Size -> FDist -> FPher -> FCoef
coefs conf n fDist fPher = (coefArr !) 
  where
    b = if returnToOrigin conf > 0 then 0 else 1 -- HACK
    coefArr = listArray ((b, b), (n, n)) lst :: CArray
    lst = [val (x, y) | x <- [b..n], y <- [b..n]] 
    val (x, y) = (fPher (x, y) ** paramAlpha conf) / (dist (x, y) ** paramBeta conf)
    dist (0, 0) = 50 -- HACK
    dist (x, y) = fDist (x, y)

findPath :: Size -> FCoef -> ACOm Path
findPath n coef = do
  randOrig <- asks originRandom -- HACK
  v <- asks returnToOrigin -- HACK
  origin <- if randOrig then getRandomR (1, n) else return 0 -- HACK
  let sta = if randOrig then Set.delete origin (Set.fromAscList [1..n]) else Set.fromAscList [1..n]
      zrs = if randOrig then [] else replicate v 0
      st = (sta, origin, zrs)
  res <- lift . execWriterT . flip evalStateT st . fix $ \ loop -> do
    (unv, lst, orgs) <- get
    nxt <- lift . lift . pickNext (\ x -> coef (lst, x)) $ (Set.elems unv) ++ orgs -- HACK
    let unv' = if nxt == 0 then unv else Set.delete nxt unv
        orgs' = if nxt == 0 then tail orgs else orgs -- HACK
    put (unv', nxt, orgs')
    tell [nxt]
    unless (Set.null unv' && null orgs' ) loop 
  return $ (origin:res) ++ [origin]

pickNext :: (Vertex -> Coefficient) -> [Vertex] -> Rand StdGen Vertex
pickNext coef unv = do
  let list = map (coef &&& id) unv :: [(Probability, Vertex)]
      norm = foldl' ((. fst) . (+)) 0 list
  r <- (* norm) <$> getRandom
  return . flip evalState (0, list) . fix $ \ loop -> do
    (s,(p,v):ts) <- get
    let ns = s + p
    put (ns, ts)
    if r <= ns 
      then return v
      else loop

allPaths :: Size -> FCoef -> Int -> ACOm [Path]
allPaths n coef s = execWriterT . flip evalStateT s . fix $ \ loop -> do
  i <- get
  path <- lift . lift $ findPath n coef
  tell [path]
  put $ i - 1
  unless (i == 1) loop

bestPath :: FDist -> FPher -> ACOm (Distance, Path)
bestPath fDist fPher = do
  conf <- ask
  n <- asks paramSize
  ants <- asks paramAGen
  res <- allPaths n (coefs conf n fDist fPher) $ ants
  return . minimum . map (\ p -> (penalty conf p (pathLen fDist p), p)) $ res

pathLen :: FDist -> Path -> Distance
pathLen fDist path = foldl' ((. fDist) . (+)) 0 (zip path (tail path))

generations :: DArray -> ACOm [(Distance, Path)] 
generations dist = do
  conf <- ask
  n <- asks paramSize
  nthRoot <- (** (1.0 / fromIntegral n)) <$> asks paramPBest
  let pBestCoef = (1.0 - nthRoot) / (((fromIntegral n / 2.0) - 1.0) * nthRoot) :: Pheromon
  initPh <- newPhrArray
  let heuristic = if paramUse2Opt conf then use2opt (dist !) else id
  execWriterT . flip evalStateT (initPh, 0) . forever $ do
    (pher, mp) <- get
    bp <- lift . lift $ heuristic <$> bestPath (dist !) (pher !) 
    tell [bp]
    let minP = if mp == 0 then fst bp else min mp (fst bp)
    newPher <- lift . lift $ updatePheromones pBestCoef minP bp pher
    put (newPher, minP)
    
use2opt :: FDist -> (Distance, Path) -> (Distance, Path)
use2opt dist (_, path) = let newPath = Topt.optimize dist path in (pathLen dist newPath, newPath)

optimize :: ConfigACO -> DArray -> IO (Distance, Path)
optimize config dist = do
  gens <- evalRandIO $ runReaderT (generations dist) config
  return . minimum . take (paramNGen config) $ gens

optimizeWithInfo :: ConfigACO -> DArray -> IO ((Distance, Path), [Distance])
optimizeWithInfo config dist = do
  gens <- evalRandIO $ runReaderT (generations dist) config
  let res = take (paramNGen config) $ gens
  return (minimum res, map fst res)
