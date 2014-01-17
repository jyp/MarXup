{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards, PostfixOperators, LiberalTypeSynonyms, TypeOperators, OverloadedStrings #-}

module MarXup.DerivationTrees (
-- * Basics
module Data.Monoid,
module Data.LabeledTree,

-- * Derivation' building
-- axiom, rule, etc, aborted, 
emptyDrv, haltDrv, haltDrv', abortDrv, delayPre, 
dummy, rule, Derivation, Premise, Rule(..), 

-- * Links
Alignment(..), LineStyle(..),defaultLink,Link(..),

-- * Figure building
Figure(..),

-- * Engine
derivationTree, derivationTreeMP

) where

-- import DerivationTrees.Basics
import Data.List
import Data.Traversable hiding (mapM)
import Control.Monad.Writer 
import Control.Applicative 
import Data.LabeledTree
-- import Control.Applicative.State
import Data.Monoid hiding ((<>))
import MarXup.Tex hiding (label)
import MarXup.Latex (math)
import MarXup.MultiRef
import MarXup.MetaPost

------------------
--- Basics

data LineStyle = None | Simple | Double | Dotted | Dashed | Waved | TeXDotted
  deriving (Enum,Show,Eq,Ord)

data Link = Link {label :: Tex (), linkStyle :: LineStyle, align :: Alignment, steps :: Int}  -- ^ Regular link
          | Detached {label :: Tex ()}               -- ^ Detach the derivation as another figure
          | Delayed {align :: Alignment} -- ^ automatic delaying
-- deriving Show

defaultLink :: Link
defaultLink = Link mempty Dotted CenterA 0

data Alignment = LeftA | CenterA | RightA
  deriving (Eq,Ord)
           
instance Show Alignment where    
    show LeftA = "l"
    show CenterA = "c"
    show RightA = "r"


-------------------

data Rule tag = Rule {tag :: tag, ruleStyle :: LineStyle, delimiter :: Tex (), ruleLabel :: Tex (), conclusion :: Tex ()}
--  deriving Show

{-
instance Monoid t => Applicative (Writer t) where
    pure = return
    (<*>) = ap
-}

type Premise = Link ::> Derivation' ()
type Derivation' tag = Tree Link (Rule tag) 
type Derivation = Derivation' ()

data Figure tag = Figure {figureTag :: Label, contents :: Derivation' tag}

------------------------------------------------------------
-- Phase 1: Detach

type Detach x = x -> WriterT [Figure ()] Tex x

detachP :: Detach Premise
detachP (Detached{..} ::> d) = do
  d'@(Node r ps) <- detachD d
  figureTag <- lift $ newLabel 
  tell [Figure {contents = Node r {delimiter = label} ps,..}]
  return $ (defaultLink ::> haltDrv label d)
detachP (l ::> d) = (l ::>) <$> detachD d

detachD :: Detach Derivation
detachD (Node n ps) = Node n <$> for ps detachP

detachF :: Figure () -> WriterT [Figure ()] Tex ()
detachF Figure{..} = do
  contents <- detachD contents
  tell [Figure{..}]

-- | Detach figures which should be detached.
detachTop :: [Figure ()] -> Tex [Figure ()]
detachTop fs = do 
  figs <- runWriterT $ for fs detachF
  return $ snd $ figs

--------------------------------------------------
-- Phase 2: Delay

insertAt n x xs = take n xs ++ x : drop n xs

rm idx [] = []
rm 0 (x:xs) = xs
rm n (x:xs) = x : rm (n-1) xs

depth (Detached{} ::> _) = 2
depth (Link{steps} ::> Node _ ps) = 1 + steps + maximum (0 : map depth ps)

isDelayed :: Premise -> Bool
isDelayed (Delayed{} ::> _) = True
isDelayed _ = False

delayPre a s (Link {..} ::> j) = Link {steps = s, align = a, ..} ::> j

delayD :: Derivation -> Derivation
delayD (Node r ps0) = Node r (map delayP ps)
    where ps = fmap (fmap delayD) ps0
          ps' = filter (not . isDelayed) ps
          delayP (Delayed{..} ::> d) = Link{..} ::> d
             where steps = 1 + maximum (0 : map depth ps')
                   label = mempty
                   linkStyle = Dotted
          delayP p = p

delayF :: Figure () -> Figure ()
delayF (Figure{..}) = Figure{contents = delayD contents,..}

delayTop = map delayF



---------------------------------------------------------
-- Phase 3: Tag

type Tag x = x () -> Tex (x Int)

tagify :: Tag Rule
tagify (Rule {..}) = do
  tag <- newLabel
  return $ Rule {..}

tagifyFig :: Tag Figure
tagifyFig (Figure {..}) = Figure figureTag <$> traverse tagify contents

tagifyTop :: [Figure ()] -> Tex [Figure Int]
tagifyTop = mapM tagifyFig

----------------------------------------------------------
-- Phase 4: Metapostify


mkTuple :: [MP ()] -> MP ()
mkTuple l = " (" <> sequence_ (intersperse "," l) <> ") "

link (Link {..} ::> Node (Rule{tag}) _) 
    | steps == 0 = sho tag
    | otherwise = "MVD " <> sho tag <> " " <>
                  mkTuple [sho steps,sho "",mpQuote label,sho align,sho (fromEnum linkStyle)]

-- type Stringize x = x Int -> MP ()

stringize :: Derivation' Label -> MP ()
stringize (Node Rule {tag = t, ..} premises) = do
  traverse (traverse stringize) premises
  mpRaw "jgm " <> mpRefer t <> " " <> mpQuote conclusion <> ";\n"
  mpRaw "Nfr " <> mpRefer t <> mkTuple (map link premises) <> " " <>
                     mkTuple [mpQuote ruleLabel,mpQuote delimiter,mpQuote "",sho (fromEnum ruleStyle)] <> ";\n"

stringizeFig :: Figure Label -> MP () 
stringizeFig (Figure {..}) = mkfig figureTag $ do
  stringize contents
  mpRawLines ["draw drv_tree;"]

-- | Render a derivation tree. The 1st argument are the options to
-- pass to the includegraphics command.
derivationTreeMP :: [String] -> Derivation -> Tex Label
derivationTreeMP opts j = do 
  f <- newLabel
  a <- delayTop <$> detachTop [Figure f j]
  bz <- tagifyTop a   
  mapM_ (inMP . stringizeFig) bz
  includeMetaPostFigure opts f 
  return f

----------------------------------------------------------
-- Phase 4': TeXify
  
-- | Render a derivation tree without using metapost drv package (links will not be rendered properly)
derivationTree :: Derivation' a -> TeX
derivationTree = math . stringizeTex

stringizeTex :: Derivation' a -> TeX
stringizeTex (Node Rule {ruleStyle=None,..} []) = conclusion
stringizeTex (Node Rule {..} premises) = braces $ do
  cmd0 "displaystyle" -- so that the text does not get smaller
  cmdn "frac" [mconcat $ 
               intersperse (cmd0 "quad") 
               [ stringizeTex v | _ ::> v <- premises]
              ,conclusion]
  braces $ do cmd0 "small"
              ruleLabel

-----------------------

rule ruleLabel conclusion = Rule {tag = (), delimiter = mempty, ruleStyle = Simple, ..}

dummy :: Rule ()
dummy = (rule mempty mempty) {ruleStyle = None}
emptyDrv = Node dummy []

abortDrv (Node Rule {..} _) = Node Rule {ruleStyle = Waved, ..} []

-- | Used when the rest of the derivation is known.
haltDrv' :: Tex () -> Derivation -> Derivation
haltDrv' tex (Node r _) = Node r {ruleStyle = None} 
     [defaultLink {linkStyle = TeXDotted, steps = 1, label = tex} ::> emptyDrv]

-- | More compact variant
haltDrv :: Tex () -> Derivation -> Derivation
haltDrv t (Node r _) = Node r [defaultLink ::> Node dummy {conclusion = cmd "vdots" nil >> cmd "hspace" (tex "2pt") >> t} []]

