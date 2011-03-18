-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
--              TypeSynonymInstances #-} 

module Data.Alpino.DepStruct.Pickle (xpNode) where

import Control.Monad.State (State, get, put)
import Data.Tree (rootLabel, subForest)
import qualified Data.Tree as DT
import Text.XML.Expat.Pickle

import Data.Alpino.DepStruct 

data LabelOrRef =
  Label {
    labelLabel :: DSLabel
  }
  | Ref {
  refRel :: Rel,
  refIdx :: Integer
}

-- | Pickler for Alpino dependency structures.
--xpAlpinoDS :: PU [UNode String] AlpinoDS
--xpAlpinoDS = 
--  xpElemNodes "alpino_ds" $
--  xpWrap (
--    uncurry AlpinoDS,
--    \n -> (
--      dsRoot n,
--      dsSentence n
--    )
--  ) $
--  xpPair
--    xpNode
--    xpSentence

--resolveRefs :: [(Int, DSLabel)] -> DT.Tree LabelOrRef -> DT.Tree DSLabel
--resolveRefs crLabels (DT.Tree lr sf) =

type ResolveState = [(Integer, DSLabel)]


-- Resolve nodes that only have a coreference index and a relation.
resolveRefs :: DT.Tree LabelOrRef -> State ResolveState (DT.Tree DSLabel)
resolveRefs (DT.Node lr sf) = do
  label   <- resolveLabel lr
  labelSf <- mapM resolveRefs sf
  return $ DT.Node label labelSf 

resolveLabel :: LabelOrRef -> State ResolveState DSLabel
resolveLabel (Label l) = do
  coRefs <- get
  let idx = case l of
              CatLabel _ _ i   -> i
              LexLabel _ _ _ i -> i

  case idx of
    Just i  -> do
      put $ (i, l):coRefs
    Nothing -> return ()
  return $ l
resolveLabel (Ref rel idx) = do
  corefs <- get
  let corefLabel = case lookup idx corefs of
                     (Just l) -> l
                     Nothing  -> error "Invalid coreference"
  return $ case corefLabel of
            CatLabel _ _ _   -> corefLabel {catRel = rel}
            LexLabel _ _ _ _ -> corefLabel {lexRel = rel}

--

--refLabel :: DSLabel -> State ResolveState LabelOrRef
--refLabel l = do
--  labels = get
--  let idx = case l of
--              CatLabel _ _ i   -> i
--              LexLabel _ _ _ i -> i
--  case idx of
--    Just i  ->
--    Nothing -> return ()

xpCatNode :: PU [UNode String] (DT.Tree LabelOrRef)
xpCatNode =
  xpWrap (
    \((rel, cat, idx), forest) ->
      DT.Node (Label $ CatLabel rel cat idx) forest,
    \t -> (
      (
        catRel $ labelLabel $ rootLabel t,
        catCat $ labelLabel $ rootLabel t,
        catIdx $ labelLabel $ rootLabel t),
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
      (lexRel  $ labelLabel $ rootLabel t,
       lexPos  $ labelLabel $ rootLabel t,
       lexRoot $ labelLabel $ rootLabel t,
       lexIdx  $ labelLabel $ rootLabel t)) $
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
      (refRel $ rootLabel t,
       refIdx $ rootLabel t)) $
  xpElemAttrs "node"
    (xpPair
      (xpAttr "rel"   xpRel)
      (xpAttr "index" xpickle))

xpNode :: PU [UNode String] (DT.Tree LabelOrRef)
xpNode =
  xpAlt picklerIndex [xpLexNode, xpCatNode, xpRefNode]
  where
    picklerIndex (DT.Node lr _) = case lr of
      (Label label) -> case label of
        LexLabel _ _ _ _ -> 0
        CatLabel _ _ _   -> 1
      Ref _ _       -> 2

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
         Nothing -> error "Bug: Relation list is incomplete!"
      )
  xpText

xpSentence :: PU [UNode String] String
xpSentence =
  xpElemNodes "sentence" (xpContent xpText0)
