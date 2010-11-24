{-# OPTIONS -XBangPatterns #-}

module Main where

import Prelude hiding (concat, filter)

import qualified Data.Alpino.Model as AM
import Data.Alpino.Model.Enumerator
import Data.Enumerator hiding (isEOF, length, map)
import Data.List (genericLength)
import Text.Printf (printf)

statistics :: Monad m => Iteratee [AM.TrainingInstance] m (Int, Int, Int)
statistics = liftI $ step (0, 0, 0)
    where step acc@(!lenSumAcc, !lenAcc, !maxLenAcc) chunk =
              case chunk of
                Chunks [] -> Continue $ returnI . step acc
                Chunks xs -> Continue $ returnI . (step $
                             (lenSumAcc + (sum $ map length xs),
                              lenAcc + genericLength xs,
                             max maxLenAcc $ maximum $ map length xs))
                EOF -> Yield acc EOF

main :: IO ()
main = do
  (lenSum, len, maxLen) <- run_ $ lineEnum $$ joinI $ instanceParser $$
                           joinI $ groupByKey $$ statistics
  putStrLn $ "Contexts: " ++ (show len)
  putStrLn $ "Max. events: " ++ (show maxLen)
  putStrLn $ "Avg. events: " ++ (printf "%.2f" $ (fromIntegral lenSum / fromIntegral len :: Double))

