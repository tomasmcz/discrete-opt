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
  }

-- TODO better type for penalty

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

newPhrArray :: (Monad m) => ReaderT ConfigACO m PArray
newPhrArray = do
  n <- asks paramSize
  initPh <- asks paramInitPh
  return . listArray ((1, 1), (n, n)) $ repeat initPh

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
    coefArr = listArray ((1, 1), (n, n)) lst :: CArray
    lst = [val (x, y) | x <- [1..n], y <- [1..n]] 
    val (x, y) = (fPher (x, y) ** paramAlpha conf) / (fDist (x, y) ** paramBeta conf)  

findPath :: Size -> FCoef -> ACOm Path
findPath n coef = do
  randOrig <- asks originRandom
  origin <- if randOrig then getRandomR (1, n) else return 0
  let st = (Set.delete origin (Set.fromAscList [1..n]), origin)
  res <- lift . execWriterT . flip evalStateT st . fix $ \ loop -> do
    (unv, lst) <- get
    nxt <- lift . lift $ pickNext (\ x -> coef (lst, x)) (Set.elems unv) 
    let unv' = Set.delete nxt unv
    put (unv', nxt)
    tell [nxt]
    unless (Set.null unv') loop 
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
