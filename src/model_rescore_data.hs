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
                Binary           -> scoreToBinary
                BinaryNormalized -> scoreToBinaryNorm
                Normalized       -> scoreToNorm
    
  run_ $ lineEnum $$ joinI $ instanceParser $$ joinI $ groupByKey $$
       joinI $ score $$ joinI $ concat $$
       joinI $ instanceGenerator $$ printByteString

data Option = Binary | Normalized | BinaryNormalized

optionInfo :: [OptDescr Option]
optionInfo =
    [ Option ['b'] ["binary"] (NoArg Binary) "convert to binary scores",
      Option ['i'] ["binary_normalize"] (NoArg BinaryNormalized)
                 "binary normalize over context",
      Option ['n'] ["normalize"] (NoArg Normalized) "normalize over context" ]

usage :: String -> String
usage name = "Usage: " ++ name ++ " <OPTION>\n"

getOptions :: IO (Option)
getOptions = do
  args <- getArgs
  let (options, _, errors) = getOpt Permute optionInfo args
  unless (null errors && length options == 1) $ do
               name <- getProgName
               hPutStrLn stderr $ L.concat errors
               hPutStrLn stderr $ usageInfo (usage name) optionInfo
               exitFailure
  return $ head options