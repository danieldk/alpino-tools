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

readFeatureFile :: String -> IO [String]
readFeatureFile fn = do
  h <- openFile fn ReadMode
  c <- hGetContents h
  return $ lines c


readFeatureArgs :: [String] -> IO [String]
readFeatureArgs args = 
    if null args then do 
                   name <- getProgName
                   hPutStrLn stderr $ usageInfo (usage name) options
                   exitFailure
    else
        return args
      
main :: IO ()
main = do
  (opts, args) <- getOptions

  let filter0 = if optFilterFunctors opts
                then filterFeaturesFunctor
                else filterFeatures
  let filter = if optInverseFilter opts
               then filter0 not
               else filter0 id

  features <- case optFeatureFile opts of
                Just f  -> readFeatureFile f
                Nothing -> readFeatureArgs args

  let keepFeatures = Set.fromList $ map fromString features

  run_ $ lineEnum $$ joinI $ instanceParser $$
       joinI $ filter keepFeatures $$
       joinI $ instanceGenerator $$ printByteString

data Options = Options {
      optFilterFunctors :: Bool,
      optInverseFilter  :: Bool,
      optFeatureFile    :: Maybe String
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['f'] ["functor"]
                 (NoArg
                  (\opt -> opt { optFilterFunctors = True}))
                 "filter feature functors",
      Option ['i'] ["inverse"]
                 (NoArg
                  (\opt -> opt { optInverseFilter = True}))
                  "exclude specified features",
      Option ['r'] ["read"]
                  (ReqArg
                   (\arg opt -> opt { optFeatureFile = Just arg})
                   "FILE")
                  "read features from a file"]

startOptions :: Options
startOptions = Options {
                 optFilterFunctors = False,
                 optInverseFilter  = False,
                 optFeatureFile    = Nothing
}

usage :: String -> String
usage name = "Usage: " ++ name ++ " <OPTION> [FEATURES]\n"

getOptions :: IO (Options, [String])
getOptions = do
  args <- getArgs
  let (actions, keep, errors) = getOpt Permute options args
  let opts = foldl (\acc a -> a acc) startOptions actions
  return (opts, keep)
