{-# OPTIONS -XBangPatterns #-}

module Main where

import Prelude hiding (concat, filter)

import Data.Alpino.Model.Conduit
import Data.Alpino.Model.Conduit
import Data.Conduit (Sink, runResourceT, ($=), ($$))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.List (genericLength)
import System.IO (stdin)
import Text.Printf (printf)

sumCount :: (Monad m, Fractional a, Integral b) => Sink a m (a, b)
sumCount =
	CL.fold handleCtx (0, 0)
	where
		handleCtx (!scoreSum, !ctxs) score =
			(scoreSum + score, ctxs + 1)

main :: IO ()
main = do
  (scoreSum, scoreLen) <- runResourceT (CB.sourceHandle stdin $= CB.lines $=
  	bsToTrainingInstance $= groupByKey $= bestScore $$ sumCount)

  putStrLn $ "Contexts: " ++ (show scoreLen)
  putStrLn $ "Oracle: " ++ (printf("%.4f") $ scoreSum / fromIntegral scoreLen)

