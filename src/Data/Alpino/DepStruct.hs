module Data.Alpino.DepStruct (
  AlpinoDS(..),
  Cat(..),
  DSLabel(..),
  Rel(..)
) where

import Data.Tree

-- | Alpino dependency structures define syntactic relations between
--   words. For convenience, the dependency structure is represented
--   as a rose tree. Additionally, the dependency structure contains
--   the sentence corresponding to the dependency structure.
data AlpinoDS = AlpinoDS {
  -- | Root of the dependency tree. 
  dsRoot     :: Tree DSLabel,
  -- | Sentence corresponding to the dependency tree. 
  dsSentence :: String
} deriving(Show, Eq)

-- | Label containing syntactic or lexical information of a node.
data DSLabel =
   CatLabel {
    -- | Category
    catRel  :: Rel,
    -- | Dependency relation
    catCat  :: Cat
   }
  | LexLabel {
    -- | Dependency relation
    lexRel  :: Rel,
    -- | Part of speech tag
    lexPos  :: String,
    -- | Root/stem
    lexRoot :: String
  } deriving (Show, Eq)

data Rel = Hdf | Hd | Cmp | Sup | Su | Obj1 | PObj1 | Obj2| Se | PC | VC
  | SVP | PredC | Ld | Me | PredM | ObComp | Mod | Body | Det | App | Whd
  | Rhd | Cnj | Crd | Nucl | Sat | Tag | DP | Top | MWP | DLink | DashDash 
  deriving (Show, Eq)

data Cat = SMain | NP | PPart | PPres | PP | SSub | Inf | Cp | DU | Ap
  | AdvP | TI | Rel | WhRel | WhSub | Conj | WhQ | Oti | Ahi | DetP | SV1
  | SVan | MWU | TopCat
  deriving (Show, Eq)
