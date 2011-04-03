{-# OPTIONS -XBangPatterns #-}

module Main where

import Prelude hiding (concat, filter)

import qualified Data.Alpino.Model as AM
import Data.Alpino.Model.Enumerator
import Data.Enumerator hiding (isEOF, length, map)
import Data.List (genericLength)
import Text.Printf (printf)

statistics :: Monad m => Iteratee [AM.TrainingInstance] m (Int, Int, Int)
statistics = continue $ step (0, 0, 0)
    where step acc (Chunks []) = continue $ step acc
          step (!lenSumAcc, !lenAcc, !maxLenAcc) (Chunks xs) =
              continue $ (step $ (lenSumAcc + (sum $ map length xs),
                                  lenAcc + genericLength xs,
                                  max maxLenAcc $ maximum $ map length xs))
          step acc EOF = yield acc EOF

main :: IO ()
main = do
  (lenSum, len, maxLen) <- run_ $ lineEnum $$ joinI $ instanceParser $$
                           joinI $ groupByKey $$ statistics
  putStrLn $ "Contexts: " ++ (show len)
  putStrLn $ "Max. events: " ++ (show maxLen)
  putStrLn $ "Avg. events: " ++ (printf "%.2f" $ (fromIntegral lenSum / fromIntegral len :: Double))

