module Main where

import Prelude hiding (concat)

import Control.Monad (unless)
import Data.Alpino.Model.Enumerator
import Data.Enumerator (($$), joinI, run_)
import qualified Data.List as L
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  options <- getOptions
  let (SampleSize n) = head options    
  run_ $ lineEnum $$ joinI $ instanceParser $$ joinI $ groupByKey $$
       joinI $ randomSample n $$ joinI $ concat $$
       joinI $ instanceGenerator $$ printByteString

data Option = SampleSize Int

optionInfo :: [OptDescr Option]
optionInfo =
    [ Option "n" ["sample_size"] (ReqArg (\n -> SampleSize . read $ n) "NUMBER")
                 "random sample size"]

usage :: String -> String
usage name = "Usage: " ++ name ++ " <OPTION>\n"

getOptions :: IO ([Option])
getOptions = do
  args <- getArgs
  let (options, _, errors) = getOpt Permute optionInfo args
  unless (null errors && length options == 1) $ do
               name <- getProgName
               hPutStrLn stderr $ L.concat errors
               hPutStrLn stderr $ usageInfo (usage name) optionInfo
               exitFailure
  return options