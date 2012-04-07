-- |
-- Module      : Data.Alpino.Model
-- Copyright   : (c) 2010 Daniël de Kok
-- License     : Apache 2
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--
-- Data structures and functions to modify and process training data for
-- the Alpino parse disambiguation and fluency ranking components.
--
-- Since the training data follows a very general format, this module and
-- submodules should also be usable for other parsers and generators.
-- Please refer to the description of `bsToTrainingInstance` for more
-- information about the format that is used.

module Data.Alpino.Model ( FeatureValue(..),
                           TrainingInstance(..),
                           TrainingInstanceType(..),
                           bestScore,
                           bestScore',
                           bsToTrainingInstance,
                           filterFeatures,
                           filterFeaturesFunctor,
                           randomSample,
                           scoreToBinary,
                           scoreToBinaryNorm,
                           scoreToNorm,
                           trainingInstanceToBs
                         ) where

import Control.Monad (liftM)
import Control.Monad.Random.Class (MonadRandom)
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.UTF8 as BU
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import GHC.Word (Word8)
import System.Random.Shuffle (shuffleM)
import Text.Printf (printf)

-- | A training instance.
data TrainingInstance = TrainingInstance {
      instanceType     :: TrainingInstanceType, -- ^ Type of training instance
      instanceKey      :: B.ByteString,         -- ^ Training instance identifier
      instanceN        :: B.ByteString,
      instanceScore    :: Double,               -- ^ Quality score
      instanceFeatures :: Features              -- ^ Features
} deriving (Show, Eq)

-- | Type of training instance (parsing or generation).
data TrainingInstanceType = ParsingInstance
                          | GenerationInstance
    deriving (Show, Eq)

-- | Representation of features and values.
data Features = FeaturesString B.ByteString -- ^ Features as a ByteString.
              | FeaturesList [FeatureValue] -- ^ Features as a list.
                deriving (Show, Eq)

-- | A feature and its corresponding value.
data FeatureValue = FeatureValue {
      feature :: B.ByteString,
      value   :: Double
} deriving (Show, Eq)

-- | Find the highest score of a context.
bestScore :: [TrainingInstance] -> Double
bestScore = foldl (\acc -> max acc . instanceScore) 0.0

-- | Find the highest score of a context (strict).
bestScore' :: [TrainingInstance] -> Double
bestScore' = foldl' (\acc -> max acc . instanceScore) 0.0

-- |
-- Read a training instance from a `BU.ByteString`.
--
-- The bytestring is assumed to contain five fields separated by
-- the hash (/#/) character:
--
-- 1. An indicator for the type of training instance (/P/ for parse
--   disambiguation, /G/ for fluency ranking).
--
-- 2. The identifier of the context (usually the identifier of a
--   sentence of logircal form).
--
-- 3. Parse/generation number.
--
-- 4. A quality score for this training instance.
--
-- 5. A list of features and values. List elements are separated by
--   the vertical bar (/|/), and have the following form: /value@feature/
bsToTrainingInstance :: B.ByteString -> Maybe TrainingInstance
bsToTrainingInstance l
    | length lineParts /= 5 = Nothing
    | otherwise = Just $ TrainingInstance instType key n score features
    where lineParts = B.split instanceFieldSep l
          instType = bsToType $ lineParts !! 0
          key = lineParts !! 1
          n = lineParts !! 2
          score = fst . fromJust . readDouble $ lineParts !! 3
          features = FeaturesString $ lineParts !! 4

-- | Convert a training instance to a `B.ByteString`.
trainingInstanceToBs :: TrainingInstance -> B.ByteString
trainingInstanceToBs (TrainingInstance instType keyBS nBS sc fvals) =
    B.intercalate fieldSep [typeBS, keyBS, nBS, scoreBS, fValsBS]
    where typeBS = typeToBS instType
          scoreBS = BU.fromString $ printf "%f" sc
          fValsBS = featuresToBs fvals
          fieldSep = BU.fromString "#"

instanceFieldSep :: GHC.Word.Word8
instanceFieldSep = c2w '#'

bsToType :: B.ByteString -> TrainingInstanceType
bsToType bs
    | bs == parseMarker = ParsingInstance
    | bs == generationMarker = GenerationInstance
    | otherwise = error "Unknown marker."

typeToBS :: TrainingInstanceType -> B.ByteString
typeToBS ParsingInstance = parseMarker
typeToBS GenerationInstance = generationMarker

parseMarker :: BU.ByteString
parseMarker = BU.fromString "P"

generationMarker :: BU.ByteString
generationMarker = BU.fromString "G"

-- | Parsed representation of features.
parsedFeatures :: Features -> [FeatureValue]
parsedFeatures (FeaturesList l)   = l
parsedFeatures (FeaturesString s) = map fVal $ B.split fieldSep s
    where fVal p = FeatureValue f (fst $ fromJust $ readDouble valBs)
              where [valBs, f] = B.split fValSep p
          fieldSep = c2w '|'
          fValSep = c2w '@'

-- | Convert features to a bytestring.
featuresToBs :: Features -> B.ByteString
featuresToBs (FeaturesString s) = s
featuresToBs (FeaturesList l)   = B.intercalate fieldSep $ map toBs l
    where toBs (FeatureValue f val) = B.intercalate fValSep
                                      [BU.fromString $ printf "%f" val, f]
          fieldSep = BU.fromString "|" 
          fValSep  = BU.fromString "@"

-- |
-- Filter features by exact names. A modifier function can be applied,
-- for instance, the `not` function would exclude the specified features.
filterFeatures :: (Bool -> Bool) -> Set.Set B.ByteString -> TrainingInstance ->
                  TrainingInstance
filterFeatures f keepFeatures i =
    i { instanceFeatures = FeaturesList $ filter keep $
                   parsedFeatures $ instanceFeatures i}
    where keep = f . flip Set.member keepFeatures . feature

-- |
-- Filter features by their functor. A modifier function can be applied,
-- for instance, the `not` function would exclude the specified features.
filterFeaturesFunctor :: (Bool -> Bool) -> Set.Set B.ByteString ->
                         TrainingInstance -> TrainingInstance
filterFeaturesFunctor f keepFeatures i =
    i { instanceFeatures = FeaturesList $ filter keep $ parsedFeatures $
                   instanceFeatures i}
    where keep = f . flip Set.member keepFeatures . functor . feature
          functor func = B.split argOpen func !! 0
          argOpen = c2w '('

-- | Extract a random sample from a list of instances.
randomSample :: MonadRandom m => Int -> [TrainingInstance] ->
  m [TrainingInstance]
randomSample n i
  | length i <= n = return $ i
  | otherwise     = take n `liftM` shuffleM i

-- |
-- Convert the quality scores to binary scores. The instances
-- with the highest quality score get score /1.0/, other instances
-- get score /0.0/.
scoreToBinary :: [TrainingInstance] -> [TrainingInstance]
scoreToBinary ctx = map (rescoreEvt maxScore) ctx
    where maxScore = bestScore ctx
          rescoreEvt maxS evt
            | instanceScore evt == maxS = evt { instanceScore = 1.0 }
            | otherwise = evt { instanceScore = 0.0 }

-- |
-- Divide a score of /1.0/ uniformly over instances with the highest
-- quality scores.
scoreToBinaryNorm :: [TrainingInstance] -> [TrainingInstance]
scoreToBinaryNorm ctx = map (rescoreEvt maxScore) ctx
    where maxScore = bestScore ctx
          numMax = length . filter (\e -> instanceScore e == maxScore) $ ctx
          correctScore = 1.0 / fromIntegral numMax
          rescoreEvt maxS evt
            | instanceScore evt == maxS =
                evt { instanceScore = correctScore }
            | otherwise = evt { instanceScore = 0.0 }

-- | Normalize scores over all training instances.
scoreToNorm :: [TrainingInstance] -> [TrainingInstance]
scoreToNorm ctx = map (rescoreEvt norm) ctx
    where norm = sum $ map instanceScore ctx
          rescoreEvt n evt =
              evt { instanceScore = (instanceScore evt) / n }
