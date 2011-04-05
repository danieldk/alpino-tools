-- |
-- Module      : Data.Alpino.DepStruct.Pickle
-- Copyright   : (c) 2011 Daniël de Kok
-- License     : Apache 2
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--
-- Pickling and unpickling of Alpino dependency structures from XML.

module Data.Alpino.DepStruct.Pickle (xpAlpinoDS) where

import Control.Monad.State (State, evalState, get, put)
import Data.Tree (rootLabel, subForest)
import qualified Data.Tree as DT
import Text.Printf (printf)
import Text.XML.Expat.Pickle

import Data.Alpino.DepStruct 

data LabelOrRef =
  Label {
    lorLabel :: DSLabel
  }
  | Ref {
    lorRel :: Rel,
    lorIdx :: Integer
} deriving (Show)

-- | Pickler for Alpino dependency structures.
xpAlpinoDS :: PU [UNode String] AlpinoDS
xpAlpinoDS = 
  xpElemNodes "alpino_ds" $
  xpWrap (
    \(lOrRefTree, sent) ->
      AlpinoDS (evalState (resolveTree lOrRefTree) []) sent,
    \n -> (
      evalState (refTree $ dsRoot n) [],
      dsSentence n
    )
  ) $
  xpPair
    xpNode
    xpSentence

xpNode :: PU [UNode String] (DT.Tree LabelOrRef)
xpNode =
  xpAlt picklerIndex [xpLexNode, xpCatNode, xpRefNode]
  where
    picklerIndex (DT.Node lr _) = case lr of
      (Label label) -> case label of
        LexLabel _ _ _ _ -> 0
        CatLabel _ _ _   -> 1
      Ref _ _       -> 2

xpCatNode :: PU [UNode String] (DT.Tree LabelOrRef)
xpCatNode =
  xpWrap (
    \((rel, cat, idx), forest) ->
      DT.Node (Label $ CatLabel rel cat idx) forest,
    \t -> (
      (labelRel $ lorLabel $ rootLabel t,
       labelCat $ lorLabel $ rootLabel t,
       labelIdx $ lorLabel $ rootLabel t),
      subForest t)
    ) $
    xpElem "node"
    (xpTriple
      (xpAttr        "rel"   xpRel)
      (xpAttr        "cat"   xpCat)
      (xpAttrImplied "index" xpickle))
    (xpList xpNode)

xpLexNode :: PU [UNode String] (DT.Tree LabelOrRef)
xpLexNode =
  xpWrap (
    \(rel, pos, root, idx) ->
      DT.Node (Label $ LexLabel rel pos root idx) [],
    \t -> 
      (labelRel  $ lorLabel $ rootLabel t,
       labelPos  $ lorLabel $ rootLabel t,
       labelRoot $ lorLabel $ rootLabel t,
       labelIdx  $ lorLabel $ rootLabel t)) $
  xpElemAttrs "node"
    (xp4Tuple
      (xpAttr        "rel"   xpRel)
      (xpAttr        "pos"   xpText)
      (xpAttr        "root"  xpText)
      (xpAttrImplied "index" xpickle))

xpRefNode :: PU [UNode String] (DT.Tree LabelOrRef)
xpRefNode =
  xpWrap (
    \(rel, idx) ->
      DT.Node (Ref rel idx) [],
    \t ->
      (lorRel $ rootLabel t,
       lorIdx $ rootLabel t)) $
  xpElemAttrs "node"
    (xpPair
      (xpAttr "rel"   xpRel)
      (xpAttr "index" xpickle))

cats :: [(Cat, String)]
cats = [(SMain, "smain"), (NP, "np"), (PPart, "ppart"), (PPres, "ppres"),
        (PP, "pp"), (SSub, "ssub"), (Inf, "inf"), (Cp, "cp"), (DU, "du"),
        (Ap, "ap"), (AdvP, "advp"), (TI, "ti"), (Rel, "rel"), (WhRel, "whrel"),
        (WhSub, "whsub"), (Conj, "conj"), (WhQ, "whq"), (Oti, "oti"),
        (Ahi, "ahi"), (DetP, "detp"), (SV1, "sv1"), (SVan, "svan"),
        (MWU, "mwu"), (TopCat, "top")]

xpCat :: PU String Cat
xpCat =
  xpWrapMaybe_
    "Could not parse 'cat' attribute."
    (\cat -> lookup cat $ map (\(a, b) -> (b, a)) cats,
    -- Fixme: We should use pattern matching completeness check.
     \cat -> case lookup cat cats of
       Just c  -> c
       Nothing -> error "Bug: Category list is incomplete!"
    )
  xpText

rels :: [(Rel, String)]
rels = [(Hdf, "hdf"), (Hd, "hd"), (Cmp, "cmp"), (Sup, "sup"),
        (Su, "su"), (Obj1, "obj1"), (PObj1, "pobj1"), (Obj2, "obj2"),
        (Se, "se"), (PC, "pc"), (VC, "vc"), (SVP, "svp"), (PredC, "predc"),
        (Ld, "ld"), (Me, "me"), (PredM, "predm"), (ObComp, "obcomp"),
        (Mod, "mod"), (Body, "body"), (Det, "det"), (App, "app"),
        (Whd, "whd"), (Rhd, "rhd"), (Cnj, "cnj"), (Crd, "crd"),
        (Nucl, "nucl"), (Sat, "sat"), (Tag, "tag"), (DP, "dp"),
        (Top, "top"), (MWP, "mwp"), (DLink, "dlink"), (DashDash, "--")]

xpRel :: PU String Rel
xpRel =
  xpWrapMaybe_
    "Could not parse 'rel' attribute."
      (\rel -> lookup rel $ map (\(a, b) -> (b, a)) rels,
      -- Fixme: We should use pattern matching completeness check.
       \rel -> case lookup rel rels of
         Just r  -> r
         Nothing -> error "Bug: Relation list is incomplete!"
      )
  xpText

xpSentence :: PU [UNode String] String
xpSentence =
  xpElemNodes "sentence" (xpContent xpText0)

--
-- There is a discrepancy between our representation of dependency trees
-- in Haskell, and those in XML. In the XML representation, coreferent nodes
-- are only represented once in full. Subsequent occurances just have the
-- 'index' attribute.
--
-- This representation is annoying in real-life use, because a function
-- that processes a dependency structure has to resolve these 'reference
-- nodes'. For these reason, we add the full structure to all instances of
-- a coreferent node. Two functions are used:
--
-- resolveTree - expands reference nodes to full nodes (used during
--               pickling)
-- refTree     - replaces duplicate coreferent nodes by a reference (used
--               during unpickling)
--

type ResolveState = [(Integer, DT.Tree DSLabel)]

-- Resolve nodes that only have a coreference index and a relation.
resolveTree :: DT.Tree LabelOrRef -> State ResolveState (DT.Tree DSLabel)
resolveTree (DT.Node (Label l) sf) = do
  lsf <- mapM resolveTree sf
  let node = DT.Node l lsf
  case labelIdx l of
    Just idx -> do
      coIndexed <- get
      put $ (idx, node):coIndexed
    Nothing  -> return () 
  return node

resolveTree (DT.Node (Ref rel idx) _) = do
  coIndexed <- get
  let (DT.Node l ds) = case lookup idx coIndexed of
                         Just n  -> n
                         Nothing -> error $ printf "Invalid coreference: %i" idx
  let newLabel = l { labelRel = rel }
  return $ DT.Node newLabel ds

type RefState = [Integer]

refTree :: DT.Tree DSLabel -> State RefState (DT.Tree LabelOrRef)
refTree (DT.Node l sf) = do
  coIndexed <- get
  case labelIdx l of
    Just idx -> if elem idx coIndexed then 
                  return $ DT.Node (Ref (labelRel l) idx) []
                else do
                  lrSf <- mapM refTree sf
                  put $ idx : coIndexed 
                  return $ DT.Node (Label l) lrSf 
    Nothing  -> do
      lrSf <- mapM refTree sf
      return $ DT.Node (Label l) lrSf
