{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Data.Alpino.Model.Conduit (
  bestScore,
  filterFeatures,
  filterFeaturesFunctor,
  groupBy,
  groupByKey,
  bsToTrainingInstance,
  drop,
  splitWith,
  take,
  takeWhile,
  trainingInstanceToBS
) where

import           Prelude hiding (drop, take, takeWhile)
import           Control.Exception.Base (Exception)
import           Control.Monad (liftM)
import qualified Data.Alpino.Model as AM
import qualified Data.ByteString as B
import           Data.Conduit (Conduit, ConduitResult(..), Resource(..),
                   SequencedSinkResponse(Emit), Sink(..), SinkResult(..),
                   conduitState, sequenceSink, sinkState)
import qualified Data.Conduit.List as CL
import           Data.Maybe (isNothing)
import qualified Data.Set as Set
import           Data.Word (Word8)
import Data.Typeable
import Control.Monad.Trans.Resource (ResourceThrow (..))

data InvalidDataException = InvalidDataException String
  deriving Typeable

instance Exception InvalidDataException

instance Show InvalidDataException where
  show (InvalidDataException e) = show e

-- | Retrieve the best score from a list of training instances.
bestScore :: Monad m => Conduit [AM.TrainingInstance] m Double
bestScore = CL.map AM.bestScore'

-- |
-- Filter features by exact names. A modifier function can be applied,
-- for instance, the 'not' function would exclude the specified features.
filterFeatures :: Monad m => (Bool -> Bool) -> Set.Set B.ByteString ->
  Conduit AM.TrainingInstance m AM.TrainingInstance
filterFeatures f keepFeatures = CL.map (AM.filterFeatures f keepFeatures)

-- |
-- Filter features by their functor. A modifier function can be applied,
-- for instance, the 'not' function would exclude the specified features.
filterFeaturesFunctor :: Monad m => (Bool -> Bool) -> Set.Set B.ByteString ->
  Conduit AM.TrainingInstance m AM.TrainingInstance
filterFeaturesFunctor f keepFeatures =
    CL.map (AM.filterFeaturesFunctor f keepFeatures)

-- | Conduit grouping input according to an equality function.
groupBy :: Resource m => (a -> a -> Bool) -> Conduit a m [a]
groupBy f =
  conduitState
    []
    push
    close
  where
    push []      v = return ([v], Producing [])
    push s@(x:_) v
      | f x v      = return (v:s, Producing [])
      | otherwise  = return ([v],  Producing [s])
    close s = return [s]


-- | Group training instances by key.
groupByKey :: Resource m =>
           Conduit AM.TrainingInstance m [AM.TrainingInstance]
groupByKey = groupBy keyEq
    where keyEq i1 i2 = AM.instanceType i1 == AM.instanceType i2 &&
                        AM.instanceKey i1 == AM.instanceKey i2

drop :: Resource m => Int -> Sink B.ByteString m ()
drop n =
  sinkState
    n
    push
    close
  where
    push 0 x     = return (0, Done (Just x) ())
    push count v
      | bsLen <= count = return (count - bsLen, Processing)
      | otherwise      = return (0, Done (Just $ B.drop count v) ())
      where
        bsLen  = B.length v
    close _ = return ()

take :: Resource m => Int -> Sink B.ByteString m B.ByteString
take n =
  sinkState
    (n, B.empty)
    push
    close
  where
    push (0,     acc) v = return ((0, B.empty), Done (Just v) acc)
    push (count, acc) v
      | bsLen <= count  =
        return ((count - bsLen, B.append acc v), Processing)
      | otherwise       =
        let (xs, rest) = B.splitAt count v in
          return ((0, acc), Done (Just rest) (B.append acc xs))
      where
        bsLen  = B.length v
    close (_, acc) = return acc


takeWhile :: Resource m => (Word8 -> Bool) -> Sink B.ByteString m B.ByteString
takeWhile f =
  sinkState
    B.empty
    push
    close
  where
    push s v =
      if B.null rest then
        return (B.append s v, Processing)
      else
        return (B.empty, Done (Just rest) (B.append s match))
      where
        (match, rest) = B.span f v
    close s = return s

splitWith :: Resource m => (Word8 -> Bool) -> Conduit B.ByteString m B.ByteString
splitWith f =
  sequenceSink () $ \() -> do
    nextElem <- takeWhile f
    sep <- take 1 -- Drop splitting character (if any)
    eof <- isNothing `liftM` CL.peek
    if B.length sep == 1 && eof then
      return $ Emit () [nextElem, B.empty]
    else
      return $ Emit () [nextElem]

-- XXX: Use proper serialization?
bsToTrainingInstance :: ResourceThrow m => Conduit B.ByteString m AM.TrainingInstance
bsToTrainingInstance = CL.mapM cvtBS
  where
    cvtBS b =
      case AM.bsToTrainingInstance b of
        Just i -> return $ i
        Nothing -> resourceThrow $
          InvalidDataException "Could not parse instance."


-- XXX: Use some proper serialization here?
-- | Convert `TrainingInstance`s to `B.ByteString`s.
trainingInstanceToBS :: Monad m => Conduit AM.TrainingInstance m B.ByteString
trainingInstanceToBS = CL.map AM.trainingInstanceToBs
