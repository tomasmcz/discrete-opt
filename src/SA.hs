{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright    : 2014 Tomáš Musil
License      : BSD-3
Stability    : experimental
Portability  : portable 

Simulated Annealing.

-}

module SA 
  ( Score
  , Temp
  -- * Configuration
  , Config (..)
  -- * Optimization
  , optimize
  , descent
  ) where

import Control.Lens
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Writer
import System.Random ()

type Score = Double
type Prob = Double
type Temp = Double

data Config = Config { initialTemp :: Temp
                     , cooling :: Double
                     , finalTemp :: Temp
                     , coolingPeriod :: Int
                     , info :: Int
                     }

data SAState d = SAState { _temperature :: Temp
                         , _counter :: Int
                         , _pState :: (Score, d)
                         , _best :: (Score, d)
                         }

makeLenses ''SAState

optimize :: Config -> ((Score, d) -> Rand StdGen (Score, d)) -> (Score, d)
           -> Rand StdGen ((Score, d), [String])
optimize config nF init = runWriterT .
        flip evalStateT (SAState (initialTemp config) 1 init init) .
        fix $ \ loop -> do
        (SAState t i st b) <- get
        n@(nS, _) <- lift . lift $ nF st
        r <- getRandom
        when (r < prob t (fst st) nS) $ pState .= n
        newS <- use pState
        let infoText = unwords $ map show [fst newS, prob t (fst st) (fst n), t]
        bestScore <- use $ best . _1
        when (nS < bestScore) $ best .= n
        when (i `mod` coolingPeriod config == 0) $
                temperature %= (cooling config *)
        when (i `mod` info config == 0) . lift $ tell [infoText]
        counter %= (+ 1)
        nt <- use temperature
        if nt < finalTemp config
                then return b
                else loop

prob :: Temp -> Score -> Score -> Prob
prob t old new | new < old = 1
               | otherwise = exp ((old - new) / t)

descent :: ((Score, d) -> [(Score, d)]) -> (Score, d) -> ((Score, d), [String])
descent nbrsF initial = runWriter . flip evalStateT initial . fix $ \ loop -> do
        (sc, st) <- get
        let better = filter ((< sc) . fst) $ nbrsF (sc, st)
        if null better
                then return (sc, st)
                else do
                        let nxt = head better
                        put nxt
                        tell [show . fst $ nxt]
                        loop
