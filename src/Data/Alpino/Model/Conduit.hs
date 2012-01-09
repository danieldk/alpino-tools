{-# LANGUAGE DeriveDataTypeable #-}

module Data.Alpino.Model.Conduit (
  bestScore,
  filterFeatures,
  filterFeaturesFunctor,
  groupBy,
  groupByKey,
  bsToTrainingInstance,
  trainingInstanceToBS
) where

import           Control.Exception.Base (Exception)
import qualified Data.Alpino.Model as AM
import qualified Data.ByteString as B
import Data.Conduit (Conduit, ConduitResult(..), Resource(..), conduitState)
import qualified Data.Conduit.List as CL
import qualified Data.Set as Set
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
