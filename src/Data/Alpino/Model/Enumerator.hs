module Data.Alpino.Model.Enumerator ( concat,
                                      groupBy,
                                      instanceGenerator,
                                      instanceParser,
                                      lineEnum,
                                      printByteString,
                                      scoreToBinary
                                    ) where

import Prelude hiding (concat)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Alpino.Model as AM
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Enumerator as E
import Data.Enumerator hiding (isEOF, map)
import System.IO (isEOF)


instanceParser :: (Monad m) =>
                  Enumeratee BU.ByteString AM.TrainingInstance m b
instanceParser = E.map AM.bsToTrainingInstance

instanceGenerator :: (Monad m) =>
                     Enumeratee AM.TrainingInstance B.ByteString m b
instanceGenerator = E.map AM.trainingInstanceToBs

lineEnum :: MonadIO m => Enumerator B.ByteString m b
lineEnum = Iteratee . loop
    where loop (Continue k) = do
            eof <- liftIO isEOF
            case eof of
              True -> return $ Continue k
              False -> do
                       line <- liftIO B.getLine
                       runIteratee (k (Chunks [line])) >>= loop
          loop step = return step

groupBy :: (Monad m) =>
                  Enumeratee AM.TrainingInstance [AM.TrainingInstance] m b
groupBy = loop
    where loop (Continue k) = do
            h <- peek
            case h of
              Nothing -> return $ Continue k
              Just h -> do
                     xs <- E.span $ keyEq h
                     newStep <- lift $ runIteratee $ k $ Chunks [xs]
                     loop newStep
          loop step = return step
          keyEq i1 i2 = AM.instanceType i1 == AM.instanceType i2 &&
                                 AM.key i1 == AM.key i2

concat :: (Monad m) =>
              Enumeratee [a] a m b
concat = loop
    where loop (Continue k) = do
            h <- E.head
            case h of
              Nothing -> return $ Continue k
              Just h -> do
                     newStep <- lift $ runIteratee $ k $ Chunks h
                     loop newStep
          loop step = return step

printByteString :: MonadIO m => Iteratee B.ByteString m ()
printByteString = continue step
    where step (Chunks []) = continue step
          step (Chunks xs) = liftIO (mapM_ B.putStrLn xs) >> continue step
          step EOF = yield () EOF

scoreToBinary :: (Monad m) =>
              Enumeratee [AM.TrainingInstance] [AM.TrainingInstance] m b
scoreToBinary = E.map AM.scoreToBinary
