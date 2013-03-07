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
derivationTree

) where

-- import DerivationTrees.Basics
import Data.List
import Data.Traversable hiding (mapM)
import Control.Monad.Writer 
import Control.Monad.State 
import Control.Applicative 
import System.IO.Unsafe
import Data.LabeledTree
-- import Control.Applicative.State
import Data.Monoid hiding ((<>))
import MarXup.Tex hiding (label)
import MarXup.MetaPost

------------------
--- Basics

data LineStyle = None | Simple | Double | Dotted | Dashed | Waved | TeXDotted 
  deriving (Enum,Show)

data Link = Link {label :: Tex (), linkStyle :: LineStyle, align :: Alignment, steps :: Int}  -- ^ Regular link
          | Detached {label :: Tex ()}               -- ^ Detach the derivation as another figure
          | Delayed {align :: Alignment} -- ^ automatic delaying
-- deriving Show

defaultLink :: Link
defaultLink = Link mempty Dotted CenterA 0

data Alignment = LeftA | CenterA | RightA
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
-- Phase 4: Texify

mpQuote :: Tex () -> Tex ()
mpQuote t = Tex "\"" >> t >> Tex "\""

mkTuple :: [Tex ()] -> Tex ()
mkTuple l = Tex " (" >> sequence_ (intersperse "," l) >> Tex ") "

link (Link {..} ::> Node (Rule{tag}) _) 
    | steps == 0 = sho tag
    | otherwise = Tex "MVD " >> sho tag >> Tex " " >>
                  mkTuple [sho steps,sho "",mpQuote label,sho align,sho (fromEnum linkStyle)]

type Stringize x = x Int -> Tex ()

stringize :: Derivation' Label -> Tex ()
stringize (Node Rule {tag = t, ..} premises) = do
  traverse (traverse stringize) premises
  Tex "jgm " >> reference t >> Tex " " >> mpQuote conclusion >> Tex ";\n"
  Tex "Nfr " >> reference t >> mkTuple (map link premises) >> Tex " " >>
                     mkTuple [mpQuote ruleLabel,mpQuote delimiter,mpQuote (Tex ""),sho (fromEnum ruleStyle)] >> Tex ";\n"

stringizeFig :: Figure Label -> Tex () 
stringizeFig (Figure {..}) = metapostFigure figureTag $ do
  stringize contents
  texLn "draw drv_tree;"
  

------------------------------------------
-- Pipeline

-- | Render a derivation tree. The 1st argument are the options to
-- pass to the includegraphics command.
derivationTree :: [String] -> Derivation -> Tex Label
derivationTree opts j = do 
  f <- newLabel
  a <- delayTop <$> detachTop [Figure f j]
  bz <- tagifyTop a   
  Metapost $ mapM stringizeFig bz
  cmd' "includegraphics" opts (MPFigure f)
  return f

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
haltDrv t (Node r _) = Node r [defaultLink ::> Node dummy {conclusion = cmd "vdots" nil >> cmd "hspace" (Tex "2pt") >> t} []]


{-------------------------
brace :: TeX -> TeX
brace (TeX x) = TeX $ '{' : x ++ ['}']
paren (TeX x) = TeX $ '(' : x ++ [')']
brack (TeX x) = TeX $ "[" ++ x ++ "]"

tex :: String -> [TeX] -> TeX
tex macro args = TeX ('\\' : macro) <> mconcat (map brace args)
text x = tex "text" [TeX x]

f ! x = brace f <> TeX "_" <> brace x


-}
