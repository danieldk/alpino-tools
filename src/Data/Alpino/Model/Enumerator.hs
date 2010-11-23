{-# OPTIONS_GHC -XDeriveDataTypeable #-}
-- |
-- Module      : Data.Alpino.Model.Enumerator
-- Copyright   : (c) 2010 Daniël de Kok
-- License     : Apache 2
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--
-- Enumerators derived from Data.Alpino.Model

module Data.Alpino.Model.Enumerator ( bestScore,
                                      concat,
                                      groupBy,
                                      groupByKey,
                                      filter,
                                      filterFeatures,
                                      filterFeaturesFunctor,
                                      instanceGenerator,
                                      instanceParser,
                                      lineEnum,
                                      printByteString,
                                      scoreToBinary,
                                      scoreToBinaryNorm,
                                      scoreToNorm
                                    ) where

import Prelude hiding (concat, filter, head)
import Control.Exception.Base (Exception, SomeException)
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Alpino.Model as AM
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Enumerator as E
import Data.Enumerator hiding (isEOF, length, map)
import Data.List (genericLength)
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Typeable
import System.IO (isEOF)

data InvalidDataException = InvalidDataException String
                            deriving Typeable

instance Exception InvalidDataException

instance Show InvalidDataException where
    show (InvalidDataException e) = show e

-- | Retrieve the best score from a list of training instances.
bestScore :: (Monad m) =>
               Enumeratee [AM.TrainingInstance] Double m b
bestScore = E.map AM.bestScore'

-- |
-- Filter features by exact names. A modifier function can be applied,
-- for instance, the 'not' function would exclude the specified features.
filterFeatures :: (Monad m) =>  (Bool -> Bool) -> Set.Set B.ByteString ->
                  Enumeratee AM.TrainingInstance AM.TrainingInstance m b
filterFeatures mod keepFeatures = E.map (AM.filterFeatures mod keepFeatures)

-- |
-- Filter features by their functor. A modifier function can be applied,
-- for instance, the 'not' function would exclude the specified features.
filterFeaturesFunctor :: (Monad m) =>  (Bool -> Bool) -> Set.Set B.ByteString ->
                         Enumeratee AM.TrainingInstance AM.TrainingInstance m b
filterFeaturesFunctor mod keepFeatures =
    E.map (AM.filterFeaturesFunctor mod keepFeatures)

-- | Enumeratee grouping chunks according to an equality function.
groupBy :: (Monad m, Eq a) => (a -> a -> Bool) ->
           Enumeratee a [a] m b
groupBy f = loop
    where loop (Continue k) = do
            h <- peek
            case h of
              Nothing -> return $ Continue k
              Just h -> do
                     xs <- E.span $ f h
                     newStep <- lift $ runIteratee $ k $ Chunks [xs]
                     loop newStep
          loop step = return step

-- | Group training instances by key.
groupByKey :: (Monad m) =>
           Enumeratee AM.TrainingInstance [AM.TrainingInstance] m b
groupByKey = groupBy keyEq
    where keyEq i1 i2 = AM.instanceType i1 == AM.instanceType i2 &&
                        AM.key i1 == AM.key i2

-- | Enumeratee that converts ByteStrings to TrainingInstances.
instanceParser :: (Monad m) =>
                  Enumeratee BU.ByteString AM.TrainingInstance m b
instanceParser = mapMaybeEnum (InvalidDataException "Could not parse instance.")
                 AM.bsToTrainingInstance

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

-- | Enumeratee that filters with a predicate.
filter :: (Monad m) => (a -> Bool) -> Enumeratee a a m b
filter f = loop
    where loop = checkDone $ continue . step
          step k EOF = yield (Continue k) EOF
          step k (Chunks []) = continue $ step k
          step k (Chunks xs) = do
            newStep <- lift $ runIteratee $ k $ Chunks $ L.filter f xs
            loop newStep

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

mapMaybeEnum :: (Exception e, Monad m) => e -> (ao -> Maybe ai) ->
                Enumeratee ao ai m b
mapMaybeEnum exception f = loop where
    loop = checkDone $ continue . step
    step k EOF = yield (Continue k) EOF
    step k (Chunks []) = continue $ step k
    step k (Chunks xs) = case mapMaybeMaybe f xs of
                     Just ys -> k (Chunks ys) >>== loop
                     Nothing -> throwError exception
--                     Nothing -> yield (Error exception) EOF

-- If one function application fails return Nothing, otherwise Just xs
mapMaybeMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybeMaybe _ []     = Just []
mapMaybeMaybe f (x:xs) = do
  r <- f x
  rs <- mapMaybeMaybe f xs
  return $ r:rs

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

-- | Enumerator that normalized instance scores over all instances
-- | in the list.
scoreToNorm :: (Monad m) =>
               Enumeratee [AM.TrainingInstance] [AM.TrainingInstance] m b
scoreToNorm = E.map AM.scoreToNorm
