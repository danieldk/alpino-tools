{-# OPTIONS -XBangPatterns #-}

module Main where

import Prelude hiding (concat, filter)

import Data.Alpino.Model.Enumerator
import Data.Enumerator hiding (isEOF, length)
import Data.List (genericLength)
import Text.Printf (printf)

sumCount :: (Monad m, Fractional a, Integral b) => Iteratee a m (a, b)
sumCount = liftI $ step (0.0, 0)
    where step acc@(!sumAcc, !lenAcc) chunk =
              case chunk of
                Chunks [] -> Continue $ returnI . step acc
                Chunks xs -> Continue $ returnI . (step $
                             (sumAcc + sum xs, lenAcc + genericLength xs))
                EOF -> Yield acc EOF

main :: IO ()
main = do
  (scoreSum, scoreLen) <- run_ $ lineEnum $$ joinI $ instanceParser $$
                          joinI $ groupByKey $$
                          joinI $ bestScore $$ sumCount
  putStrLn $ "Contexts: " ++ (show scoreLen)
  putStrLn $ "Oracle: " ++ (printf("%.4f") $ scoreSum / fromIntegral scoreLen)

