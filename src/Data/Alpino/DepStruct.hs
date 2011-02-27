module Data.Alpino.DepStruct (
  AlpinoDS(..),
  DSLabel(..),
  Rel(..)
) where

import Data.Tree

data AlpinoDS = AlpinoDS {
  dsRoot     :: Tree DSLabel,
  dsSentence :: String
} deriving(Show, Eq)

data DSLabel =
  DSLabel {
      nodeRel  :: Rel,
      nodeCat  :: Maybe String,
      nodePos  :: Maybe String,
      nodeRoot :: Maybe String
  }
  deriving (Show, Eq)

data Rel = Hdf | Hd | Cmp | Sup | Su | Obj1 | PObj1 | Obj2| Se | PC | VC
  | SVP | PredC | Ld | Me | PredM | ObComp | Mod | Body | Det | App | Whd
  | Rhd | Cnj | Crd | Nucl | Sat | Tag | DP | Top | MWP | DLink | DashDash 
  deriving (Show, Eq)
