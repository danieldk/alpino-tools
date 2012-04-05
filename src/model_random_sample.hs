module Main where

import Prelude hiding (concat)

import Control.Monad (unless)
import Control.Monad.Random (MonadRandom(..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Alpino.Model.Conduit
import Data.Conduit (($=), ($$), runResourceT)
import qualified Data.Conduit.Binary as CB
import qualified Data.List as L
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

instance MonadRandom m => MonadRandom (ResourceT m) where
  getRandom   = lift getRandom
  getRandoms  = lift getRandoms
  getRandomR  = lift . getRandomR
  getRandomRs = lift . getRandomRs

main :: IO ()
main = do
  options <- getOptions
  let (SampleSize n) = head options    
  runResourceT (CB.sourceHandle stdin $= CB.lines $= bsToTrainingInstance $=
    groupByKey $= randomSample n $= concat $=
    trainingInstanceToBS $= addNewLine $$ CB.sinkHandle stdout)

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