{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (concat, filter)

import qualified Data.Alpino.Model as AM
import Data.Alpino.Model.Conduit
import Data.Conduit (Sink, runResourceT, ($=), ($$))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.List (genericLength)
import System.IO (stdin)
import Text.Printf (printf)

statistics :: Monad m => Sink [AM.TrainingInstance] m (Int, Int, Int)
statistics =
  CL.fold handleCtx (0, 0, 0)
  where
    handleCtx (!evts, !ctxs, !maxCtx) ctx =
      (evts + ctxLen, succ ctxs, max maxCtx ctxLen)
      where
        ctxLen =
          genericLength ctx

main :: IO ()
main = do
  (evts, ctxs, maxCtx) <- runResourceT (CB.sourceHandle stdin $=
    CB.lines $= bsToTrainingInstance $= groupByKey $$ statistics)

  putStrLn $ printf "Contexts: %d" ctxs
  putStrLn $ printf "Max. events: %d" maxCtx
  putStrLn $ printf "Avg. events: %.2f" (fromIntegral evts / fromIntegral ctxs :: Double)

