module Data.Alpino.Model ( TrainingInstance(..),
                           TrainingInstanceType(..),
                           bsToTrainingInstance,
                           scoreToBinary,
                           trainingInstanceToBs
                         ) where

import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lex.Double (readDouble)
import qualified Data.ByteString.UTF8 as BU
import Data.Maybe (fromJust)

data TrainingInstanceType =
    ParsingInstance | GenerationInstance
    deriving (Show, Eq)

data TrainingInstance = TrainingInstance {
      instanceType :: TrainingInstanceType,
      key          :: B.ByteString,
      n            :: B.ByteString,
      score        :: Double,
      features     :: B.ByteString
} deriving (Show, Eq)

bsToTrainingInstance :: B.ByteString -> TrainingInstance
bsToTrainingInstance l =
    TrainingInstance instType key n score features
    where lineParts = B.split instanceFieldSep l
          instType = bsToType $ lineParts !! 0
          key = lineParts !! 1
          n = lineParts !! 2
          score = fst . fromJust . readDouble $ lineParts !! 3
          features = lineParts !! 4

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

scoreToBinary :: [TrainingInstance] -> [TrainingInstance]
scoreToBinary ctx = map (rescoreEvt maxScore) ctx
    where maxScore = foldl (\acc e -> max acc $ score e) 0.0 ctx
          rescoreEvt maxScore evt
            | score evt == maxScore = evt { score = 1.0 }
            | otherwise = evt { score = 0.0 }
