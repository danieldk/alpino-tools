module Main where

import Prelude hiding (concat)

import Control.Monad (unless)
import Data.Alpino.Model.Conduit
import Data.ByteString.UTF8 (fromString)
import Data.Conduit (runResourceT, ($=), ($$))
import qualified Data.Conduit.Binary as CB
import Data.List as L
import qualified Data.Set as Set
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  (options, args) <- getOptions

  let filter0 = if FilterFeatures `elem` options
                then filterFeatures
                else filterFeaturesFunctor
  let filter1 = if InverseFilter `elem` options
               then filter0 not
               else filter0 id

  unless (not $ null args) $ do
         name <- getProgName
         hPutStrLn stderr $ usageInfo (usage name) optionInfo
         exitFailure

  let keepFeatures = Set.fromList $ map fromString args

  runResourceT (CB.sourceHandle stdin $= CB.lines $= bsToTrainingInstance $=
    filter1 keepFeatures $= trainingInstanceToBS $= addNewLine $$
    CB.sinkHandle stdout)

data Option = FilterFeatures | FilterFunctors | InverseFilter
            deriving Eq

optionInfo :: [OptDescr Option]
optionInfo =
    [ Option ['f'] ["functor"] (NoArg FilterFunctors) "filter feature functors",
      Option ['i'] ["inverse"] (NoArg InverseFilter) "exclude specified features"]

usage :: String -> String
usage name = "Usage: " ++ name ++ " <OPTION> [FEATURES]\n"

getOptions :: IO ([Option], [String])
getOptions = do
  args <- getArgs
  let (options, keep, errors) = getOpt Permute optionInfo args
  unless (null errors) $ do
               name <- getProgName
               hPutStrLn stderr $ L.concat errors
               hPutStrLn stderr $ usageInfo (usage name) optionInfo
               exitFailure

  case options of
    []        -> return ([FilterFeatures], keep)
    _         -> return (options, keep)
