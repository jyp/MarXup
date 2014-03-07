{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards, PostfixOperators, LiberalTypeSynonyms, TypeOperators, OverloadedStrings, PackageImports #-}

module MarXup.DerivationTrees (
-- * Basics
module Data.Monoid,
module Data.LabeledTree,

-- * Derivation' building
-- axiom, rule, etc, aborted, 
emptyDrv, haltDrv, haltDrv', delayPre,
dummy, rule, Derivation, Premise, Rule(..), 

-- * Links
LineStyle,defaultLink,Link(..),

-- * Engine
derivationTree, derivationTreeD

) where

-- import DerivationTrees.Basics
import Data.List
import Control.Monad.Writer 
import Control.Applicative 
import Data.LabeledTree
import Data.Monoid
import MarXup (element)
import MarXup.Tex hiding (label)
import MarXup.MultiRef
import MarXup.Diagram
import MarXup.Tikz as D
import qualified Data.Tree as T
------------------
--- Basics

type LineStyle = PathOptions -> PathOptions

data Link = Link {label :: Tex (), linkStyle :: LineStyle, steps :: Int}  -- ^ Regular link
          | Delayed -- ^ automatic delaying

defaultLink :: Link
defaultLink = Link mempty (denselyDotted . outline "black")  0


-------------------

data Rule = Rule {ruleStyle :: LineStyle, delimiter :: Tex (), ruleLabel :: Tex (), conclusion :: Tex ()}
--  deriving Show

type Premise = Link ::> Derivation
type Derivation = Tree Link Rule

--------------------------------------------------
-- Delay

depth (Link{steps} ::> Node _ ps) = 1 + steps + maximum (0 : map depth ps)

isDelayed :: Premise -> Bool
isDelayed (Delayed{} ::> _) = True
isDelayed _ = False

delayPre s (Link {..} ::> j) = Link {steps = s, ..} ::> j

delayD :: Derivation -> Derivation
delayD (Node r ps0) = Node r (map delayP ps)
    where ps = fmap (fmap delayD) ps0
          ps' = filter (not . isDelayed) ps
          delayP (Delayed{..} ::> d) = defaultLink {steps = 1 + maximum (0 : map depth ps')} ::> d
          delayP p = p

----------------------------------------------------------
-- TeXify
  
-- | Render a derivation tree without using metapost drv package (links will not be rendered properly)
derivationTree :: Derivation -> TeX
derivationTree = stringizeTex

stringizeTex :: Derivation -> TeX
stringizeTex (Node Rule {..} premises) = braces $ do
  cmd0 "displaystyle" -- so that the text does not get smaller
  cmdn "frac" [mconcat $
               intersperse (cmd0 "quad")
               [ stringizeTex v | _ ::> v <- premises]
              ,conclusion]
  braces $ do cmd0 "small"
              ruleLabel

----------------------------------------------------------
-- Tikzify

derivationTreeD :: Derivation -> Tex ()
derivationTreeD d = element $ derivationTreeDiag $ delayD d

derivationTreeDiag :: Derivation -> Diagram ()
derivationTreeDiag d = do
  [h] <- newVars [ContVar] -- the height of a layer in the tree.
  minimize h
  h >== 1
  tree@(T.Node (_,n,_) _) <- toDiagram h d
  forM_ (T.levels tree) $ \ls ->
    case ls of
      [] -> return ()
      (_:ls') -> forM_ (zip ls ls') $ \((_,_,l),(r,_,_)) ->
        (l + Point 10 0) `westOf` r
  let leftFringe = map head nonNilLevs
      rightFringe = map last nonNilLevs
      nonNilLevs = filter (not . null) $ T.levels tree
  [leftMost,rightMost] <- newVars [ContVar,ContVar]
  forM_ leftFringe $ \(p,_,_) ->
    leftMost <== xpart p
  forM_ rightFringe $ \(_,_,p) ->
    xpart p <== rightMost
  minimize $ 10 *- (rightMost - leftMost)
  n # Center .=. Point 0 0

toDiagPart :: Expr -> Premise -> Diagram (T.Tree (Point,Object,Point))
toDiagPart layerHeight (Link{..} ::> rul)
  | steps == 0 = toDiagram layerHeight rul
  | otherwise = do
    above@(T.Node (_,concl,_) _) <- toDiagram layerHeight rul
    ptObj <- vrule
    let pt = ptObj # S
    pt `eastOf` (concl # W)
    pt `westOf` (concl # E)
    xpart pt =~= xpart (concl # Center)
    let top = ypart (concl # S)
    ypart pt + (fromIntegral steps *- layerHeight) === top
    using linkStyle $ path $ polyline [ptObj # Base,Point (xpart pt) top]
    let embedPt 1 x = T.Node (concl # W,ptObj,concl # E) [x]
        embedPt n x = T.Node (pt,ptObj,pt) [embedPt (n-1) x]
    return $ embedPt steps above

-- | @chainBases distance objects@
-- - Ensures that all the objects have the same baseline.
-- - Separates the objects by the given distance
-- - Returns an object encompassing the group, with a the baseline set correctly.
-- - Returns the average distance between the objects
   
chainBases :: Expr -> [Object] -> Diagram (Object,Expr)
chainBases _ [] = do
  o <- abstractBox
  return (o,0)
chainBases spacing ls = do
  grp <- abstractBox
  forM_ [Base,N,S] $ \ anch -> do
    D.align ypart $ map (# anch) (grp:ls)
  dxs <- forM (zip ls (tail ls)) $ \(x,y) -> do
    let dx = xdiff (x # E) (y # W)
    dx >== spacing
    return dx
  D.align xpart [grp # W,head ls # W]
  D.align xpart [grp # E,last ls # E]
  return (grp,avg dxs)

-- | Make an horizontally flexible box of glue.
mkHGlue :: Expr -> Expr -> Diagram Object
mkHGlue minimumWidth preferredWidth = do
  g <- abstractBox
  width g >== minimumWidth
  width g =~= preferredWidth
  return g

-- | Put object in a box of the same vertical extent, and baseline,
-- but whose height can be bigger.
relaxHeight o = do
  b <- abstractBox
  -- using (outline "green")$ traceBounds o
  D.align xpart [b#W,o#W]
  D.align xpart [b#E,o#E]
  D.align ypart [b#Base,o#Base]
  o `fitsVerticallyIn` b
  return b

toDiagram :: Expr -> Derivation -> Diagram (T.Tree (Point,Object,Point))
toDiagram layerHeight (Node Rule{..} premises) = do
  ps <- mapM (toDiagPart layerHeight) premises
  concl <- relaxHeight =<< extend 1.5 <$> texObj conclusion
  -- using (outline "red")$ traceBounds concl
  lab <- texObj ruleLabel

  -- Grouping
  (psGrp,premisesDist) <- chainBases 10 [p | T.Node (_,p,_) _ <- ps]
  -- using (outline "blue" . denselyDotted) $ traceBounds psGrp
  height psGrp === layerHeight

  -- Sepaartion rule
  separ <- hrule
  separ # N .=. psGrp # S
  align ypart [concl # N,separ # S]
  minimize $ width separ
  psGrp `fitsHorizontallyIn` separ
  concl `fitsHorizontallyIn` separ

  -- rule label
  lab # BaseW .=. separ # E + Point 3 (negate 1)


  -- layout hints (not necessary for "correctness")
  let xd = xdiff (separ # W) (psGrp # W)
  xd   === xdiff (psGrp # E) (separ # E) 
  relax 2 $ (2 *- xd) =~= premisesDist
  -- centering of conclusion
  xd' <- absoluteValue $ xdiff (separ # Center) (concl # Center)
  relax 3 $ minimize xd'

  -- draw the rule.
  localPathOptions ruleStyle $ path $ polyline [separ # W,separ # E]
  return $ T.Node (separ # W, concl, lab # E) ps

-----------------------


rule ruleLabel conclusion = Rule {delimiter = mempty, ruleStyle = outline "black", ..}

dummy :: Rule
dummy = (rule mempty mempty) {ruleStyle = const defaultPathOptions}
emptyDrv = Node dummy []

-- abortDrv (Node Rule {..} _) = Node Rule {ruleStyle = Waved, ..} []

-- | Used when the rest of the derivation is known.
haltDrv' :: Tex () -> Derivation -> Derivation
haltDrv' tex (Node r _) = Node r {ruleStyle = noOutline}
     [defaultLink {steps = 1, label = tex} ::> emptyDrv]

-- | More compact variant
haltDrv :: Tex () -> Derivation -> Derivation
haltDrv t (Node r _) = Node r [defaultLink ::> Node dummy {conclusion = cmd "vdots" nil >> cmd "hspace" (tex "2pt") >> t} []]

