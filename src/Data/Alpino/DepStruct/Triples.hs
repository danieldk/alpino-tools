-- |
-- Module      : Data.Alpino.DepStruct.Triples
-- Copyright   : (c) 2011 Daniël de Kok
-- License     : Apache 2
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--
-- Definition and extraction of Alpino dependency triples.

module Data.Alpino.DepStruct.Triples (

  -- * Dependency triples
  DepTriple(..),
  DepTripleComponent(..),
  depTriples,

  -- * Utility functions
  tzFold

) where

import Control.Monad (ap)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set, fromList)
import Data.Tree.Zipper

import Data.Alpino.DepStruct

-- Dependency triples

-- | The 'DepTriple' type represents a dependency that occurs in
--   a dependency structure. The triple consists of the head, a dependent, and
--   the relation between the head and the dependeny. For convenience, the
--   triple is composed of two 'DepTripleComponent' instances: the first
--   representing the head and its role in the relation, the second
--   representing the dependant and its role in the relation.
data DepTriple = DepTriple {
  tripleHead :: DepTripleComponent,
  tripleDep  :: DepTripleComponent
} deriving (Eq, Ord, Show)

-- | The 'DepTripleComponent' type represents a head or a dependant in a
--   dependency relation.
data DepTripleComponent = DepTripleComponent {
  triplePos  :: String,
  tripleRoot :: String,
  tripleRel  :: Rel
} deriving (Eq, Ord, Show)

-- | Extract 'DepTriples' from the tree starting at the node represented by
--   the 'TreePos' zipper.
depTriples :: TreePos Full DSLabel -> Set DepTriple
depTriples =
  fromList . map (uncurry hdDepToTriple) . hdsDeps . heads
  where
    hdsDeps = concatMap hdDeps           -- Find dependencies of given heads.
    hdDeps = (zip . repeat) `ap` dependants -- Find dependencies of a head.

heads :: TreePos Full DSLabel -> [TreePos Full DSLabel]
heads =
  tzFilter isHead

isHead :: TreePos Full DSLabel -> Bool
isHead t = case label t of
  (LexLabel rel _ _ _ _ _) -> rel `elem` headRels
  _                    -> False

headRels :: [Rel]
headRels = [Hd, Cmp, Crd, DLink, Rhd, Whd]

-- Find dependants of a node. Dependants are:
--
-- * Siblings that are lexical nodes
-- * Heads of non-lexical nodes
--
dependants :: TreePos Full DSLabel -> [TreePos Full DSLabel]
dependants = mapMaybe lexOrHdDtr . siblings

-- Get zippers for the siblings of a node.
siblings :: TreePos Full DSLabel ->
  [TreePos Full DSLabel]
siblings t =
  case parent t of
    (Just p) -> filter (t /=) $ childList p
    Nothing  -> [] -- No parent? No siblings.

-- Get zippers fo the children of a node.
childList :: TreePos Full DSLabel -> [TreePos Full DSLabel]
childList = curLevel . firstChild
  where
    curLevel (Nothing) = []
    curLevel (Just t') = t':curLevel (next t')

-- If the node is a lexical node, return it as-is. If not, return its
-- head daughter.
lexOrHdDtr :: TreePos Full DSLabel -> Maybe (TreePos Full DSLabel)
lexOrHdDtr t = 
  case label t of
    (LexLabel {}) -> Just t
    (CatLabel {}) -> case filter isHead $ childList t of
                           [c] -> Just c
                           _   -> Nothing

-- Retrieve the relation of a node if it serves as a dependent.
relAsDependent :: TreePos Full DSLabel -> Maybe Rel
relAsDependent t =
  case label t of
    (LexLabel rel _ _ _ _ _) -> if rel `elem` headRels then
                              case parent t of
                                Just p -> case label p of
                                  (LexLabel rel' _ _ _ _ _) -> Just rel'
                                  (CatLabel rel' _ _ _ _)   -> Just rel'
                                Nothing -> Nothing
                            else
                             Just rel
    (CatLabel {})            -> Nothing

hdDepToTriple :: TreePos Full DSLabel -> TreePos Full DSLabel ->
  DepTriple
hdDepToTriple hd dep = DepTriple hdTripleComp depTripleComp
  where
    hdTripleComp = DepTripleComponent (labelPos hdLabel) (labelRoot hdLabel) (labelRel hdLabel)
    hdLabel = label hd
    depTripleComp = DepTripleComponent (labelPos depLabel) (labelRoot depLabel) (fromJust $ relAsDependent dep)
    depLabel = label dep

-- Utility functions

-- | Fold over a tree depth-first, starting at the node wrapped in the
--   'TreePos' zipper.
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

