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
  option <- getOptions
  let score = case option of
                Normalized   -> scoreToBinaryNorm
                UnNormalized -> scoreToBinary
    
  run_ $ lineEnum $$ joinI $ instanceParser $$ joinI $ groupByKey $$
       joinI $ score $$ joinI $ concat $$
       joinI $ instanceGenerator $$ printByteString

data Option = Normalized | UnNormalized

optionInfo :: [OptDescr Option]
optionInfo =
    [ Option ['n'] ["normalize"] (NoArg Normalized) "normalize over context" ]

usage :: String -> String
usage name = "Usage: " ++ name ++ " <OPTION>\n"

getOptions :: IO (Option)
getOptions = do
  args <- getArgs
  let (options, _, errors) = getOpt Permute optionInfo args
  unless (null errors) $ do
               name <- getProgName
               hPutStrLn stderr $ L.concat errors
               hPutStrLn stderr $ usageInfo (usage name) optionInfo
               exitFailure
  case options of
    [] -> return (UnNormalized)
    otherwise -> return $ head options