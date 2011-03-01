module Data.Alpino.DepStruct (
  AlpinoDS(..),
  Cat(..),
  DepTriple(..),
  DepTripleComponent(..),
  DSLabel(..),
  Rel(..),
  depTriples,
  heads,
  relAsDependent,
  siblings,
  tzFold,
  dependants
) where

import Data.Maybe
import Data.Tree
import Data.Tree.Zipper

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

tzFold :: (a -> TreePos Full b -> a) -> a -> TreePos Full b -> a
tzFold f acc t =
  foldSiblings $ foldChildren $ f acc t
  where
    foldChildren acc' =
      case firstChild t of
        Just c  -> tzFold f acc' c
        Nothing -> acc'
    foldSiblings acc' =
      case next t of
        Just s  -> tzFold f acc' s
        Nothing -> acc'

tzFilter :: (TreePos Full b -> Bool) -> TreePos Full b -> [TreePos Full b]
tzFilter f =
  tzFold adder []
  where
    adder acc t
      | f t       = t:acc
      | otherwise = acc

heads :: TreePos Full DSLabel -> [TreePos Full DSLabel]
heads =
  tzFilter isHead

isHead :: TreePos Full DSLabel -> Bool
isHead t = case label t of
  (LexLabel rel _ _) -> rel `elem` headRels
  _                  -> False

headRels :: [Rel]
headRels = [Hd, Cmp, Crd, DLink, Rhd, Whd]

-- Find dependants of a node. Dependants are:
--
-- * Siblings that are lexical nodes
-- * Heads of non-lexical nodes
--
dependants :: TreePos Full DSLabel -> [TreePos Full DSLabel]
dependants = map lexOrHdDtr . siblings

-- Get zippers for the siblings of a node.
siblings :: TreePos Full DSLabel ->
  [TreePos Full DSLabel]
siblings t =
  case parent t of
    (Just p) -> filter ((/=) t) $ childList p
    Nothing  -> [] -- No parent? No siblings.

-- Get zippers fo the children of a node.
childList :: TreePos Full DSLabel -> [TreePos Full DSLabel]
childList = curLevel . firstChild
  where
    curLevel (Nothing) = []
    curLevel (Just t') = t':curLevel (next t')

-- If the node is a lexical node, return it as-is. If not, return its
-- head daughter.
lexOrHdDtr :: TreePos Full DSLabel -> TreePos Full DSLabel
lexOrHdDtr t = 
  case label t of
    (LexLabel _ _ _) -> t
    (CatLabel _ _)   -> case filter isHead $ childList t of
                           [c] -> c
                           _   -> error "Malformed tree."

-- Retrieve the relation of a node if it serves as a dependent.
relAsDependent :: TreePos Full DSLabel -> Maybe Rel
relAsDependent t =
  case label t of
    (LexLabel rel _ _) -> if rel `elem` headRels then
                            case parent t of
                              Just p -> case label p of
                                (LexLabel rel' _ _) -> Just rel'
                                (CatLabel rel' _)   -> Just rel'
                              Nothing -> Nothing
                          else
                            Just rel
    (CatLabel _ _)     -> Nothing

depTriples :: TreePos Full DSLabel -> [DepTriple]
depTriples t = foldl depTriples_ [] tHeadsDeps 
  where tHeads     = heads t
        deps       = map dependants tHeads
        tHeadsDeps = zip tHeads deps

depTriples_ :: [DepTriple] -> (TreePos Full DSLabel, [TreePos Full DSLabel]) ->
  [DepTriple]
depTriples_ acc (_, []) = acc
depTriples_ acc (hd, dep:xs) =
  depTriples_ (hdDepToTriple hd dep:acc) (hd, xs)


hdDepToTriple :: TreePos Full DSLabel -> TreePos Full DSLabel ->
  DepTriple
hdDepToTriple hd dep = DepTriple hdTripleComp depTripleComp
  where
    hdTripleComp = DepTripleComponent (lexPos hdLabel) (lexRoot hdLabel) (lexRel hdLabel)
    hdLabel = label hd
    depTripleComp = DepTripleComponent (lexPos depLabel) (lexRoot depLabel) (fromJust $ relAsDependent dep)
    depLabel = label dep

data DepTriple = DepTriple {
  tripleHead :: DepTripleComponent,
  tripleDep  :: DepTripleComponent
} deriving (Eq, Show)

data DepTripleComponent = DepTripleComponent {
  triplePos  :: String,
  tripleRoot :: String,
  tripleRel  :: Rel
} deriving (Eq, Show)
