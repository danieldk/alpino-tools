{-# OPTIONS -XBangPatterns #-}

module Main where

import Prelude hiding (concat, filter)

import Control.Monad
import Data.Alpino.Model.Enumerator
import qualified Data.ByteString as B
import Data.Enumerator hiding (isEOF, length)
import qualified Data.Map as M
import Numeric.MaxEnt.Train (estimate)

main :: IO ()
main = do
  corpus <- run_ $ lineEnum $$ joinI $ instanceParser $$
            joinI $ groupByKey $$
            joinI $ trainingContextToContext $$ consume
  result <- estimate corpus
  case result of
    Left error    -> putStrLn $ show error
    Right weights -> forM_ (M.toList weights) printFeature

printFeature :: (B.ByteString, Double) -> IO ()
printFeature (f, w) = do
  B.putStr f
  putStrLn $ "|" ++ show w
  

