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
                                      randomSample,
                                      scoreToBinary,
                                      scoreToBinaryNorm,
                                      scoreToNorm
                                    ) where

import Prelude hiding (concat, filter, head, mapM)
import Control.Exception.Base (Exception)
import qualified Control.Monad as CM
import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Data.Alpino.Model as AM
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Enumerator as E
import Data.Enumerator hiding (isEOF, length, map)
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Typeable
import System.IO (isEOF)
import System.Random (getStdRandom, split)

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
filterFeatures f keepFeatures = E.map (AM.filterFeatures f keepFeatures)

-- |
-- Filter features by their functor. A modifier function can be applied,
-- for instance, the 'not' function would exclude the specified features.
filterFeaturesFunctor :: (Monad m) =>  (Bool -> Bool) -> Set.Set B.ByteString ->
                         Enumeratee AM.TrainingInstance AM.TrainingInstance m b
filterFeaturesFunctor f keepFeatures =
    E.map (AM.filterFeaturesFunctor f keepFeatures)

-- | Enumeratee grouping chunks according to an equality function.
groupBy :: (Monad m, Eq a) => (a -> a -> Bool) ->
           Enumeratee a [a] m b
groupBy f = loop
    where loop (Continue k) = do
            h <- peek
            case h of
              Nothing -> return $ Continue k
              Just e -> do
                     xs <- E.span $ f e
                     newStep <- lift $ runIteratee $ k $ Chunks [xs]
                     loop newStep
          loop step = return step

-- | Group training instances by key.
groupByKey :: (Monad m) =>
           Enumeratee AM.TrainingInstance [AM.TrainingInstance] m b
groupByKey = groupBy keyEq
    where keyEq i1 i2 = AM.instanceType i1 == AM.instanceType i2 &&
                        AM.instanceKey i1 == AM.instanceKey i2

-- | Enumeratee that converts `BU.ByteString` to `AM.TrainingInstance`.
instanceParser :: (Monad m) =>
                  Enumeratee BU.ByteString AM.TrainingInstance m b
instanceParser = mapMaybeEnum (InvalidDataException "Could not parse instance.")
                 AM.bsToTrainingInstance

-- | Enumeratee that converts `AM.TrainingInstance` to `B.ByteString`.
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
              Just e -> do
                     newStep <- lift $ runIteratee $ k $ Chunks e
                     loop newStep
          loop step = return step


mapM :: Monad m => (ao -> m ai) -> Enumeratee ao ai m b
mapM f = loop where
    loop = checkDone $ continue . step
    step k EOF = yield (Continue k) EOF
    step k (Chunks []) = continue $ step k
    step k (Chunks xs) = ( do
                             ys <- lift $ CM.mapM f xs
                             k $ Chunks ys
                         ) >>== loop

mapMaybeEnum :: (Exception e, Monad m) => e -> (ao -> Maybe ai) ->
                Enumeratee ao ai m b
mapMaybeEnum exception f = loop where
    loop = checkDone $ continue . step
    step k EOF = yield (Continue k) EOF
    step k (Chunks []) = continue $ step k
    step k (Chunks xs) = case mapMaybeMaybe f xs of
                     Just ys -> k (Chunks ys) >>== loop
                     Nothing -> throwError exception

-- If one function application fails return Nothing, otherwise Just xs
mapMaybeMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybeMaybe _ []     = Just []
mapMaybeMaybe f (x:xs) = do
  r <- f x
  rs <- mapMaybeMaybe f xs
  return $ r:rs

-- | Iterator printing `B.ByteString` to the standard output.
printByteString :: MonadIO m => Iteratee B.ByteString m ()
printByteString = continue step
    where step (Chunks []) = continue step
          step (Chunks xs) = liftIO (mapM_ B.putStrLn xs) >> continue step
          step EOF = yield () EOF

-- | Extract a random sample of @n@ instances from a context.
randomSample :: (MonadIO m) => Int ->
                Enumeratee [AM.TrainingInstance] [AM.TrainingInstance] m b
randomSample n = mapM (liftIO . sampleFun)
    where sampleFun :: [AM.TrainingInstance] -> IO [AM.TrainingInstance]
          sampleFun i = do
            gen <- getStdRandom split
            return $ AM.randomSample gen n i

-- |
-- Enumerator recaculating scores to binary scores (/1.0/ for best,
-- /0.0/ for the rest).
scoreToBinary :: (Monad m) =>
                 Enumeratee [AM.TrainingInstance] [AM.TrainingInstance] m b
scoreToBinary = E.map AM.scoreToBinary

-- |
-- Enumerator recalculating scores, dividing a score of /1.0/ uniformly
-- over instances with the highest quality score.
scoreToBinaryNorm :: (Monad m) =>
                     Enumeratee [AM.TrainingInstance] [AM.TrainingInstance] m b
scoreToBinaryNorm = E.map AM.scoreToBinaryNorm

-- |
-- Enumerator that normalized instance scores over all instances
-- in the list.
scoreToNorm :: (Monad m) =>
               Enumeratee [AM.TrainingInstance] [AM.TrainingInstance] m b
scoreToNorm = E.map AM.scoreToNorm
