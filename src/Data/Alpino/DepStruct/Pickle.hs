-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
--              TypeSynonymInstances #-} 

module Data.Alpino.DepStruct.Pickle (xpAlpinoDS) where

import Data.Tree (rootLabel, subForest)
import qualified Data.Tree as DT
import Text.XML.Expat.Pickle

import Data.Alpino.DepStruct 

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

xpNode :: PU [UNode String] (DT.Tree DSLabel)
xpNode =
  xpWrap (
    \((rel, cat, pos, root), forest) ->
      DT.Node (DSLabel rel cat pos root) forest,
    \t -> (
      (
        nodeRel $ rootLabel  t,
        nodeCat $ rootLabel  t,
        nodePos $ rootLabel  t,
        nodeRoot $ rootLabel t),
      subForest t)
  ) $
  xpElem "node"
    (xp4Tuple
      (xpAttr        "rel"  xpRel)
      (xpAttrImplied "cat"  xpText)
      (xpAttrImplied "pos"  xpText)
      (xpAttrImplied "root" xpText))
    (xpList xpNode)


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
  xpWrap (
    \rel -> case lookup rel $ map (\(a, b) -> (b, a)) rels of
      Just r  -> r
      Nothing -> DashDash,
    \rel -> case lookup rel rels of
      Just r  -> r
      Nothing -> "--"
  )
  xpText

xpSentence :: PU [UNode String] String
xpSentence =
  xpElemNodes "sentence" (xpContent xpText0)
