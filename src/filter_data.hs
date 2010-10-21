module Main where

import Prelude hiding (concat)

import Control.Monad (unless)
import Data.Alpino.Model.Enumerator
import Data.ByteString.UTF8 (fromString)
import Data.Enumerator (($$), joinI, run_)
import qualified Data.Set as Set
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  unless (not $ null args) $ do
         name <- getProgName
         hPutStrLn stderr $ usage name
         exitFailure

  let keepFeatures = Set.fromList $ map fromString args

  run_ $ lineEnum $$ joinI $ instanceParser $$
       joinI $ filterFeatures keepFeatures $$
       joinI $ instanceGenerator $$ printByteString

usage :: String -> String
usage name = "Usage: " ++ name ++ " [Features]\n"