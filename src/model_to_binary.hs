module Main where

import Prelude hiding (concat)
import Data.Alpino.Model.Enumerator
import Data.Enumerator (($$), joinI, run_)

main = do
  run_ $ lineEnum $$ joinI $ instanceParser $$ joinI $ groupBy $$
       joinI $ scoreToBinary $$ joinI $ concat $$
       joinI $ instanceGenerator $$ printByteString
