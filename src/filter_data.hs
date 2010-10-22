module Main where

import Prelude hiding (concat)

import Control.Monad (unless)
import Data.Alpino.Model.Enumerator
import Data.ByteString.UTF8 (fromString)
import Data.Enumerator (($$), joinI, run_)
import Data.List as L
import qualified Data.Set as Set
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  (option, args) <- getOptions

  let filter = case option of
                 FilterFeatures -> filterFeatures
                 FilterFunctors -> filterFeaturesFunctor

  unless (not $ null args) $ do
         name <- getProgName
         hPutStrLn stderr $ usageInfo (usage name) optionInfo
         exitFailure

  let keepFeatures = Set.fromList $ map fromString args

  run_ $ lineEnum $$ joinI $ instanceParser $$
       joinI $ filter keepFeatures $$
       joinI $ instanceGenerator $$ printByteString

data Option = FilterFeatures | FilterFunctors

optionInfo :: [OptDescr Option]
optionInfo =
    [ Option ['f'] ["functor"] (NoArg FilterFunctors) "filter feature functors" ]

usage :: String -> String
usage name = "Usage: " ++ name ++ " <OPTION> [FEATURES]\n"

getOptions :: IO (Option, [String])
getOptions = do
  args <- getArgs
  let (options, keep, errors) = getOpt Permute optionInfo args
  unless (null errors) $ do
               name <- getProgName
               hPutStrLn stderr $ L.concat errors
               hPutStrLn stderr $ usageInfo (usage name) optionInfo
               exitFailure

  case options of
    []        -> return (FilterFeatures, keep)
    otherwise -> return (head options, keep)