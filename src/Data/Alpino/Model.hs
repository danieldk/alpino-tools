-- |
-- Module      : Data.Alpino.Model
-- Copyright   : (c) 2010 Daniël de Kok
-- License     : Apache 2
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--
-- Data structures and functions to modify and process training data for
-- the Alpino parser/generator.

module Data.Alpino.Model ( TrainingInstance(..),
                           TrainingInstanceType(..),
                           bestScore,
                           bestScore',
                           bsToTrainingInstance,
                           filterFeatures,
                           filterFeaturesFunctor,
                           scoreToBinary,
                           scoreToBinaryNorm,
                           scoreToNorm,
                           trainingInstanceToBs
                         ) where

import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.UTF8 as BU
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Text.Printf (printf)

-- | A training instance.
data TrainingInstance = TrainingInstance {
      instanceType :: TrainingInstanceType, -- ^ Type of training instance
      key          :: B.ByteString,         -- ^ Training instance identifier
      n            :: B.ByteString,
      score        :: Double,               -- ^ Quality score
      features     :: Features              -- ^ Features
} deriving (Show, Eq)

-- | Type of training instance (parsing or generation)/
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
bestScore = foldl (\acc e -> max acc $ score e) 0.0

-- | Find the highest score of a context (strict).
bestScore' :: [TrainingInstance] -> Double
bestScore' = foldl' (\acc e -> max acc $ score e) 0.0

-- | Read a training instance from a ByteString.
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

-- | Convert a training instance to a ByteString.
trainingInstanceToBs :: TrainingInstance -> B.ByteString
trainingInstanceToBs (TrainingInstance instType keyBS nBS sc fvals) =
    B.intercalate fieldSep [typeBS, keyBS, nBS, scoreBS, fValsBS]
    where typeBS = typeToBS instType
          scoreBS = BU.fromString $ printf "%f" sc
          fValsBS = featuresToBs fvals
          fieldSep = BU.fromString "#"

instanceFieldSep = c2w '#'

bsToType :: B.ByteString -> TrainingInstanceType
bsToType bs
    | bs == parseMarker = ParsingInstance
    | bs == generationMarker = GenerationInstance

typeToBS :: TrainingInstanceType -> B.ByteString
typeToBS ParsingInstance = parseMarker
typeToBS GenerationInstance = generationMarker

parseMarker = BU.fromString "P"
generationMarker = BU.fromString "G"

-- | Parsed representation of features.
parsedFeatures :: Features -> [FeatureValue]
parsedFeatures f@(FeaturesList l) = l
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
-- for instance, the 'not' function would exclude the specified features.
filterFeatures :: (Bool -> Bool) -> Set.Set B.ByteString -> TrainingInstance ->
                  TrainingInstance
filterFeatures mod keepFeatures i =
    i { features = FeaturesList $ filter keep $
                   parsedFeatures $ features i}
    where keep fv = mod $ Set.member (feature fv) keepFeatures

-- |
-- Filter features by their functor. A modifier function can be applied,
-- for instance, the 'not' function would exclude the specified features.
filterFeaturesFunctor :: (Bool -> Bool) -> Set.Set B.ByteString ->
                         TrainingInstance -> TrainingInstance
filterFeaturesFunctor mod keepFeatures i =
    i { features = FeaturesList $ filter keep $ parsedFeatures $ features i}
    where keep fv = mod $ Set.member (functor $ feature fv) keepFeatures
          functor f = B.split argOpen f !! 0
          argOpen = c2w '('

-- |
-- Convert the quality scores to binary scores. The instances
-- with the highest quality score get score 1.0, other instances
-- get score 0.0.
scoreToBinary :: [TrainingInstance] -> [TrainingInstance]
scoreToBinary ctx = map (rescoreEvt maxScore) ctx
    where maxScore = bestScore ctx
          rescoreEvt maxScore evt
            | score evt == maxScore = evt { score = 1.0 }
            | otherwise = evt { score = 0.0 }

-- |
-- Divide a score of 1.0 uniformly over instances with the highest
-- quality scores.
scoreToBinaryNorm :: [TrainingInstance] -> [TrainingInstance]
scoreToBinaryNorm ctx = map (rescoreEvt maxScore) ctx
    where maxScore = bestScore ctx
          numMax = length . filter (\e -> score e == maxScore) $ ctx
          correctScore = 1.0 / fromIntegral numMax
          rescoreEvt maxScore evt
            | score evt == maxScore = evt { score = correctScore }
            | otherwise = evt { score = 0.0 }

-- | Normalize scores over all training instances.
scoreToNorm :: [TrainingInstance] -> [TrainingInstance]
scoreToNorm ctx = map (rescoreEvt norm) ctx
    where norm = sum $ map score ctx
          rescoreEvt norm evt = evt { score = (score evt) / norm }
