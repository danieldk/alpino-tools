module Data.Alpino.Model.Enumerator ( concat,
                                      groupBy,
                                      instanceGenerator,
                                      instanceParser,
                                      lineEnum,
                                      printByteString,
                                      scoreToBinary,
                                      scoreToBinaryNorm
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

-- | Enumeratee that converts ByteStrings to TrainingInstances.
instanceParser :: (Monad m) =>
                  Enumeratee BU.ByteString AM.TrainingInstance m b
instanceParser = E.map AM.bsToTrainingInstance

-- | Enumeratee that converts TrainingInstances to ByteStrings.
instanceGenerator :: (Monad m) =>
                     Enumeratee AM.TrainingInstance B.ByteString m b
instanceGenerator = E.map AM.trainingInstanceToBs

-- | Enumerator of lines read from the standard input.
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

-- | Group training instances by type and key.
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

-- | Enumeratee concatenating lists.
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

-- | Iterator printing ByteStrings to the standard output.
printByteString :: MonadIO m => Iteratee B.ByteString m ()
printByteString = continue step
    where step (Chunks []) = continue step
          step (Chunks xs) = liftIO (mapM_ B.putStrLn xs) >> continue step
          step EOF = yield () EOF

-- | Enumerator recaculating scores to binary scores (1.0 for best,
-- | 0.0 for the rest).
scoreToBinary :: (Monad m) =>
                 Enumeratee [AM.TrainingInstance] [AM.TrainingInstance] m b
scoreToBinary = E.map AM.scoreToBinary

-- | Enumerator recalculating scores, dividing a score of 1.0 uniformly
-- | over instances with the highest quality score.
scoreToBinaryNorm :: (Monad m) =>
                     Enumeratee [AM.TrainingInstance] [AM.TrainingInstance] m b
scoreToBinaryNorm = E.map AM.scoreToBinaryNorm
