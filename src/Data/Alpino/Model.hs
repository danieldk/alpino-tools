module Data.Alpino.Model ( TrainingInstance(..),
                           TrainingInstanceType(..),
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
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Text.Printf (printf)

-- | A training instance.
data TrainingInstance = TrainingInstance {
      instanceType :: TrainingInstanceType, -- ^ Type of training instance
      key          :: B.ByteString,         -- ^ Training instance identifier
      n            :: B.ByteString,
      score        :: Double,               -- ^ Quality score
      features     :: [FeatureValue]        -- ^ Features
} deriving (Show, Eq)

-- | Type of training instance (parsing or generation)
data TrainingInstanceType =
    ParsingInstance | GenerationInstance
    deriving (Show, Eq)

data FeatureValue = FeatureValue {
      feature :: B.ByteString,
      value   :: Double
} deriving (Show, Eq)

-- | Read a training instance from a ByteString.
bsToTrainingInstance :: B.ByteString -> TrainingInstance
bsToTrainingInstance l =
    TrainingInstance instType key n score features
    where lineParts = B.split instanceFieldSep l
          instType = bsToType $ lineParts !! 0
          key = lineParts !! 1
          n = lineParts !! 2
          score = fst . fromJust . readDouble $ lineParts !! 3
          features = bsToFeatureValue $ lineParts !! 4

-- | Convert a training instance to a ByteString.
trainingInstanceToBs :: TrainingInstance -> B.ByteString
trainingInstanceToBs (TrainingInstance instType keyBS nBS sc fvals) =
    B.intercalate fieldSep [typeBS, keyBS, nBS, scoreBS, fValsBS]
    where typeBS = typeToBS instType
          scoreBS = BU.fromString $ printf "%f" sc
          fValsBS = featureValueToBs fvals
          fieldSep = BU.fromString "#"

instanceFieldSep = c2w '#'

bsToType :: B.ByteString -> TrainingInstanceType
bsToType bs
    | bs == parseMarker = ParsingInstance
    | bs == generationMarker = GenerationInstance

typeToBS :: TrainingInstanceType -> B.ByteString
typeToBS instanceType
    | instanceType == ParsingInstance = parseMarker
    | instanceType == GenerationInstance = generationMarker

parseMarker = BU.fromString "P"
generationMarker = BU.fromString "G"

bsToFeatureValue :: B.ByteString -> [FeatureValue]
bsToFeatureValue = map fVal . B.split fieldSep
    where fVal p = FeatureValue f (fst $ fromJust $ readDouble valBs)
              where [valBs, f] = B.split fValSep p
          fieldSep = c2w '|'
          fValSep = c2w '@'

featureValueToBs :: [FeatureValue] -> B.ByteString
featureValueToBs = B.intercalate fieldSep . map toBs
    where toBs (FeatureValue f val) = B.intercalate fValSep
                                      [BU.fromString $ printf "%f" val, f]
          fieldSep = BU.fromString "|" 
          fValSep  = BU.fromString "@"

filterFeatures :: Set.Set B.ByteString -> TrainingInstance ->
                  TrainingInstance
filterFeatures keepFeatures i = i { features = filter keep $ features i}
    where keep fv = Set.member (feature fv) keepFeatures

filterFeaturesFunctor :: Set.Set B.ByteString -> TrainingInstance ->
                         TrainingInstance
filterFeaturesFunctor keepFeatures i = i { features = filter keep $ features i}
    where keep fv = Set.member (functor $ feature fv) keepFeatures
          functor f = B.split argOpen f !! 0
          argOpen = c2w '('

-- | Convert the quality scores to binary scores. The instances
-- | with the highest quality score get score 1.0, other instances
-- | get score 0.0.
scoreToBinary :: [TrainingInstance] -> [TrainingInstance]
scoreToBinary ctx = map (rescoreEvt maxScore) ctx
    where maxScore = foldl (\acc e -> max acc $ score e) 0.0 ctx
          rescoreEvt maxScore evt
            | score evt == maxScore = evt { score = 1.0 }
            | otherwise = evt { score = 0.0 }

-- | Divide a score of 1.0 uniformly over instances with the highest
-- | quality scores.
scoreToBinaryNorm :: [TrainingInstance] -> [TrainingInstance]
scoreToBinaryNorm ctx = map (rescoreEvt maxScore) ctx
    where maxScore = foldl (\acc e -> max acc $ score e) 0.0 ctx
          numMax = length . filter (\e -> score e == maxScore) $ ctx
          bestScore = 1.0 / fromIntegral numMax
          rescoreEvt maxScore evt
            | score evt == maxScore = evt { score = bestScore }
            | otherwise = evt { score = 0.0 }

-- | Normalize scores over all training instances.
scoreToNorm :: [TrainingInstance] -> [TrainingInstance]
scoreToNorm ctx = map (rescoreEvt norm) ctx
    where norm = sum $ map score ctx
          rescoreEvt norm evt = evt { score = (score evt) / norm }