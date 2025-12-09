{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards, PostfixOperators, LiberalTypeSynonyms, TypeOperators, OverloadedStrings, PackageImports, ScopedTypeVariables #-}

module MarXup.DerivationTrees (
-- * Basics
module Data.Monoid,
module Data.LabeledTree,

-- * Engine
haltDrv,
module Graphics.Diagrams.DerivationTrees,

-- * Frontend
derivationTree, derivationTreeD

) where

-- import DerivationTrees.Basics
import Data.List
import Data.LabeledTree
import Data.Monoid
import MarXup (element)
import MarXup.Tex hiding (label)
import Graphics.Diagrams.DerivationTrees
import MarXup.Diagram

----------------------------------------------------------
-- Tikzify

derivationTreeD :: Derivation TeX -> TeX
derivationTreeD d = element $ (derivationTreeDiag $ delayD d :: TexDiagram ())

----------------------------------------------------------
-- TeXify
  
-- | Render a derivation tree using simple latex only (links will not be rendered properly)
derivationTree :: Derivation TeX -> TeX
derivationTree = stringizeTex

stringizeTex :: Derivation TeX -> TeX
stringizeTex (Node Rule {..} premises) = braces $ do
  cmd0 "displaystyle" -- so that the text does not get smaller
  braces $ do cmd0 "small"
              leftLabel
  _ <- cmdn "frac" [mconcat $
                    intersperse (cmd0 "quad")
                    [ stringizeTex v | _ ::> v <- premises]
                   ,conclusion]
  braces $ do cmd0 "small"
              rightLabel

-- -- | More compact variant
haltDrv :: TeX -> Derivation TeX -> Derivation TeX
haltDrv t (Node r _) = Node r [defaultLink ::> Node dummy {conclusion = cmd "vdots" nil >> cmd "hspace" (tex "2pt") >> t} []]

