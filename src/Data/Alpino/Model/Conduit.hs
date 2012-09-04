{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}

module Data.Alpino.Model.Conduit (
  addNewLine,
  bestScore,
  concat,
  filterFeatures,
  filterFeaturesFunctor,
  groupByKey,
  randomSample,
  scoreToBinary,
  scoreToBinaryNorm,
  scoreToNorm,
  bsToTrainingInstance,
  trainingInstanceToBS
) where

import           Prelude hiding (concat)
import           Control.Exception.Base (Exception)
import           Control.Monad.Random.Class (MonadRandom(..))
import qualified Data.Alpino.Model as AM
import qualified Data.ByteString as B
import           Data.Conduit (Conduit)
import           Data.Conduit.Util (ConduitStateResult(StateProducing),
                                    conduitState)
import qualified Data.Conduit.List as CL
import qualified Data.Set as Set
import Data.Typeable
import Control.Monad.Trans.Resource (MonadThrow (..))

data InvalidDataException = InvalidDataException String
  deriving Typeable

instance Exception InvalidDataException

instance Show InvalidDataException where
  show (InvalidDataException e) = show e

addNewLine :: Monad m => Conduit B.ByteString m B.ByteString
addNewLine =
  CL.map (`B.snoc` 10)

-- | Retrieve the best score from a list of training instances.
bestScore :: Monad m => Conduit [AM.TrainingInstance] m Double
bestScore = CL.map AM.bestScore'

concat :: Monad m => Conduit [a] m a
concat =
  conduitState
    ()
    (\_ v -> return $ StateProducing () v)
    (\_   -> return [])


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

-- | Group training instances by key.
groupByKey :: Monad m =>
           Conduit AM.TrainingInstance m [AM.TrainingInstance]
groupByKey = CL.groupBy keyEq
    where keyEq i1 i2 = AM.instanceType i1 == AM.instanceType i2 &&
                        AM.instanceKey i1 == AM.instanceKey i2

randomSample :: MonadRandom m => Int ->
  Conduit [AM.TrainingInstance] m [AM.TrainingInstance]
randomSample size = CL.mapM $ AM.randomSample size

-- |
-- Conduit recaculating scores to binary scores (/1.0/ for best,
-- /0.0/ for the rest).
scoreToBinary :: Monad m =>
  Conduit [AM.TrainingInstance] m [AM.TrainingInstance]
scoreToBinary = CL.map AM.scoreToBinary

-- |
-- Conduit recalculating scores, dividing a score of /1.0/ uniformly
-- over instances with the highest quality score.
scoreToBinaryNorm :: Monad m =>
  Conduit [AM.TrainingInstance] m [AM.TrainingInstance]
scoreToBinaryNorm = CL.map AM.scoreToBinaryNorm

-- |
-- Conduit that normalized instance scores over all instances
-- in the list.
scoreToNorm :: Monad m =>
  Conduit [AM.TrainingInstance] m [AM.TrainingInstance]
scoreToNorm = CL.map AM.scoreToNorm

-- XXX: Use proper serialization?
bsToTrainingInstance :: MonadThrow m => Conduit B.ByteString m AM.TrainingInstance
bsToTrainingInstance = CL.mapM cvtBS
  where
    cvtBS b =
      case AM.bsToTrainingInstance b of
        Just i -> return i
        Nothing -> monadThrow $
          InvalidDataException "Could not parse instance."

-- XXX: Use some proper serialization here?
-- | Convert `TrainingInstance`s to `B.ByteString`s.
trainingInstanceToBS :: Monad m => Conduit AM.TrainingInstance m B.ByteString
trainingInstanceToBS = CL.map AM.trainingInstanceToBs
