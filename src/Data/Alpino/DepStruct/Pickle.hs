-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
--              TypeSynonymInstances #-} 

module Data.Alpino.DepStruct.Pickle (xpAlpinoDS) where

import Data.Tree (rootLabel, subForest)
import qualified Data.Tree as DT
import Text.XML.Expat.Pickle

import Data.Alpino.DepStruct 

-- | Pickler for Alpino dependency structures.
xpAlpinoDS :: PU [UNode String] AlpinoDS
xpAlpinoDS = 
  xpElemNodes "alpino_ds" $
  xpWrap (
    uncurry AlpinoDS,
    \n -> (
      dsRoot n,
      dsSentence n
    )
  ) $
  xpPair
    xpNode
    xpSentence

xpCatNode :: PU [UNode String] (DT.Tree DSLabel)
xpCatNode =
  xpWrap (
    \((rel, cat), forest) ->
      DT.Node (CatLabel rel cat) forest,
    \t -> (
      (
        catRel $ rootLabel t,
        catCat $ rootLabel t),
      subForest t)
    ) $
    xpElem "node"
    (xpPair
      (xpAttr "rel" xpRel)
      (xpAttr "cat" xpCat))
    (xpList xpNode)

xpLexNode :: PU [UNode String] (DT.Tree DSLabel)
xpLexNode =
  xpWrap (
    \(rel, pos, root) ->
      DT.Node (LexLabel rel pos root) [],
    \t -> 
      (lexRel $ rootLabel  t,
       lexPos $ rootLabel  t,
       lexRoot $ rootLabel t)) $
  xpElemAttrs "node"
    (xpTriple
      (xpAttr        "rel"  xpRel)
      (xpAttr        "pos"  xpText)
      (xpAttr        "root" xpText))

xpNode :: PU [UNode String] (DT.Tree DSLabel)
xpNode =
  xpAlt picklerIndex [xpLexNode, xpCatNode]
  where
    picklerIndex (DT.Node label _) = case label of
      LexLabel _ _ _ -> 0
      CatLabel _ _   -> 1

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
       Nothing -> error "Bug: Relation list is incomplete!") $
  xpText

rels :: [(Rel, String)]
rels = [(Hdf, "hdf"), (Hd, "hd"), (Cmp, "cmp"), (Sup, "sup"),
        (Su, "su"), (Obj1, "obj1"), (PObj1, "pobj1"), (Obj2, "obj2"),
        (Se, "se"), (PC, "pc"), (VC, "vc"), (SVP, "svp"), (PredC, "predc"),
        (Ld, "ld"), (Me, "me"), (PredM, "predm"), (ObComp, "obcomp"),
        (Mod, "mod"), (Body, "body"), (Det, "det"), (App, "app"),
        (Whd, "whd"), (Rhd, "rhd"), (Cnj, "Cnj"), (Crd, "Crd"),
        (Nucl, "Nucl"), (Sat, "sat"), (Tag, "tag"), (DP, "dp"),
        (Top, "top"), (MWP, "mwp"), (DLink, "dlink"), (DashDash, "--")]

xpRel :: PU String Rel
xpRel =
  xpWrapMaybe_
    "Could not parse 'rel' attribute."
      (\rel -> lookup rel $ map (\(a, b) -> (b, a)) rels,
      -- Fixme: We should use pattern matching completeness check.
       \rel -> case lookup rel rels of
         Just r  -> r
         Nothing -> "--"
      )
  xpText

xpSentence :: PU [UNode String] String
xpSentence =
  xpElemNodes "sentence" (xpContent xpText0)
