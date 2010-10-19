module Data.Alpino.Model ( TrainingInstance(..),
                           TrainingInstanceType(..),
                           bsToTrainingInstance,
                           scoreToBinary,
                           scoreToBinaryNorm,
                           trainingInstanceToBs
                         ) where

import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.UTF8 as BU
import Data.Maybe (fromJust)


-- | A training instance.
data TrainingInstance = TrainingInstance {
      instanceType :: TrainingInstanceType, -- ^ Type of training instance
      key          :: B.ByteString,         -- ^ Training instance identifier
      n            :: B.ByteString,
      score        :: Double,               -- ^ Quality score
      features     :: B.ByteString          -- ^ Features
} deriving (Show, Eq)

-- | Type of training instance (parsing or generation)
data TrainingInstanceType =
    ParsingInstance | GenerationInstance
    deriving (Show, Eq)

-- | Read a training instance from a ByteString.
bsToTrainingInstance :: B.ByteString -> TrainingInstance
bsToTrainingInstance l =
    TrainingInstance instType key n score features
    where lineParts = B.split instanceFieldSep l
          instType = bsToType $ lineParts !! 0
          key = lineParts !! 1
          n = lineParts !! 2
          score = fst . fromJust . readDouble $ lineParts !! 3
          features = lineParts !! 4

-- | Convert a training instance to a ByteString.
trainingInstanceToBs :: TrainingInstance -> B.ByteString
trainingInstanceToBs i = B.append typeBS $ B.append fieldSep $
                         B.append keyBS $ B.append fieldSep $
                         B.append nBS $ B.append fieldSep $
                         B.append scoreBS $ B.append fieldSep featuresBS
    where typeBS = typeToBS $ instanceType i
          keyBS = key i
          nBS = n i
          scoreBS = BU.fromString . show $ score i
          featuresBS = features i
          fieldSep = BU.fromString "#"

instanceFieldSep = c2w '#'

bsToType :: B.ByteString -> TrainingInstanceType
bsToType bs
    | bs == parseMarker = ParsingInstance
    | bs == generationMarker = GenerationInstance

parseMarker = BU.fromString "P"
generationMarker = BU.fromString "G"

typeToBS :: TrainingInstanceType -> B.ByteString
typeToBS instanceType
    | instanceType == ParsingInstance = parseMarker
    | instanceType == GenerationInstance = generationMarker

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
