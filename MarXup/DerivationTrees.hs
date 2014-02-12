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
derivationTree, derivationTreeMP, derivationTreeD

) where

-- import DerivationTrees.Basics
import Data.List
import Data.Traversable hiding (mapM)
import Control.Monad.Writer 
import Control.Applicative 
import Data.LabeledTree
-- import Control.Applicative.State
import Data.Monoid hiding ((<>))
import MarXup (element)
import MarXup.Tex hiding (label)
import MarXup.Latex (math)
import MarXup.MultiRef
-- import MarXup.MetaPost hiding ((===), alignVert, xpart, ypart, Expr)
import MarXup.MetaPost (MP(..),sho,mpRefer,mpRaw,mpQuote,mkfig,inMP,mpRawLines,includeMetaPostFigure)
import MarXup.Diagram
import MarXup.Tikz as D hiding (None)
import qualified Data.Tree as T
------------------
--- Basics

data LineStyle = None | Simple | Double | Dotted | Dashed | Waved | TeXDotted
  deriving (Enum,Show,Eq,Ord)

data Link = Link {label :: Tex (), linkStyle :: LineStyle, align :: Alignment, steps :: Int}  -- ^ Regular link
          | Detached {label :: Tex ()}   -- ^ Detach the derivation as another figure
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

type Premise = Premise' ()
type Premise' a = Link ::> Derivation' a
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

----------------------------------------------------------
-- Phase 4'': Tikzify

derivationTreeD :: Derivation -> Tex ()
derivationTreeD d = element $ derivationTreeDiag $ delayD d
  
derivationTreeDiag :: Derivation' a -> Diagram ()
derivationTreeDiag d = do
  [h] <- newVars [ContVar] -- the height of a layer in the tree.
  minimize h
  h >== 1
  tree@(T.Node (_,n,_) _) <- toDiagram h d
  forM_ (T.levels tree) $ \ls ->
    case ls of
      [] -> return ()
      (_:ls') -> forM_ (zip ls ls') $ \((_,_,l),(r,_,_)) ->
        (l + Point 4 0) `westOf` r
  let leftFringe = map head nonNilLevs
      rightFringe = map last nonNilLevs
      nonNilLevs = filter (not . null) $ T.levels tree
  [leftMost,rightMost] <- newVars [ContVar,ContVar]
  forM_ leftFringe $ \(p,_,_) ->
    leftMost <== xpart p
  forM_ rightFringe $ \(_,_,p) ->
    xpart p <== rightMost
  minimize $ 10 *- (rightMost - leftMost)
  n Center .=. Point 0 0

toDiagPart :: Expr -> Premise' a -> Diagram (T.Tree (Point,Object,Point))
toDiagPart layerHeight (Link{..} ::> rul)
  | steps == 0 = toDiagram layerHeight rul
  | otherwise = do
    above@(T.Node (_,concl,_) _) <- toDiagram layerHeight rul
    ptObj <- abstractPoint
    let pt = ptObj Center
    pt `eastOf` concl W
    pt `westOf` concl E
    xpart pt =~= xpart (concl Center)
    let top = ypart (concl S)
    ypart pt + (fromIntegral steps *- layerHeight) === top
    draw $ polyline [pt,Point (xpart pt) top]
    let embedPt 1 x = T.Node (concl W,ptObj,concl E) [x]
        embedPt n x = T.Node (pt,ptObj,pt) [embedPt (n-1) x]
    return $ embedPt steps above

-- | chainBases distance objects
-- - Ensures that all the objects have the same baseline.
-- - Separates the objects by the given distance
-- - Returns an object encompassing the group, with a correctly set baseline.
chainBases :: Expr -> [Object] -> Diagram Object
chainBases _ [] = abstractBox
chainBases spacing ls = do
  grp <- abstractBox
  D.align ypart $ map ($ Base) (grp:ls)
  forM_ (zip ls (tail ls)) $ \(x,y) -> (x E + Point spacing 0) `westOf` (y W)
  forM_ ls $ \l -> grp `taller` l
  D.align xpart [grp W,head ls W]
  D.align xpart [grp E,last ls E]
  -- drawBounds grp
  return grp

toDiagram :: Expr -> Derivation' a -> Diagram (T.Tree (Point,Object,Point))
toDiagram layerHeight (Node Rule{..} premises) = do
  ps <- mapM (toDiagPart layerHeight) premises
  concl <- extend 1.5 <$> texObj (math conclusion)
  lab <- texObj (math ruleLabel)
  psGrp <- chainBases 10 $ [p | T.Node (_,p,_) _ <- ps]
  layerHeight === height psGrp
  separ <- abstractBox
  separ N .=. psGrp S
  concl N .=. separ S
  lab BaseW .=. separ E + Point 3 (negate 2)
  height separ === 0
  thinest separ
  separ `wider` psGrp
  separ `wider` concl
  alignVert [separ Center,concl Center]
  when (ruleStyle /= None) $ draw $ polyline [separ W,separ E]
  return $ T.Node (separ W, concl, lab E) ps

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

