{-# OPTIONS -XBangPatterns #-}

module Main where

import Prelude hiding (concat, filter)

import Control.Monad
import Data.Alpino.Model.Enumerator
import qualified Data.ByteString as B
import Data.Enumerator hiding (isEOF, length)
import qualified Data.Map as M
import Numeric.MaxEnt.Train (estimate)
import Numeric.MaxEnt.Train.Enumerator (toTinyEst)
import System (getArgs)
import System.IO (IOMode(..), hClose, hPutStrLn, openFile, stderr)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2 then error "Invalid number of arguments!" else return ()
  let [fn, fn2] = args
  h1 <- openFile fn WriteMode
  h2 <- openFile fn2 WriteMode
  run_ $ lineEnum $$ joinI $ instanceParser $$
            joinI $ groupByKey $$
            joinI $ trainingContextToContext $$ toTinyEst h1 h2
  hClose h1
  hClose h2
  --result <- estimate corpus
  --case result of
  --  Left error    -> putStrLn $ show error
  --  Right weights -> forM_ (M.toList weights) printFeature

printFeature :: (B.ByteString, Double) -> IO ()
printFeature (f, w) = do
  B.putStr f
  putStrLn $ "|" ++ show w
