{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards, PostfixOperators, LiberalTypeSynonyms, TypeOperators, OverloadedStrings, PackageImports, ScopedTypeVariables #-}

module MarXup.DerivationTrees (
-- * Basics
module Data.Monoid,
module Data.LabeledTree,

-- * Derivation' building
-- axiom, rule, etc, aborted, 
emptyDrv, haltDrv', delayPre,
dummy, rule, Derivation, Premise, Rule(..), 

-- * Links
LineStyle,defaultLink,Link(..),

-- * Engine
derivationTree, derivationTreeD

) where

-- import DerivationTrees.Basics
import Data.List
import Control.Monad.Writer 
import Data.LabeledTree
import Data.Monoid
import MarXup (element)
import MarXup.Tex hiding (label)
import MarXup.Diagram as D
import qualified Data.Tree as T
------------------
--- Basics

type LineStyle = PathOptions -> PathOptions

data Link lab = Link {label :: lab, linkStyle :: LineStyle, steps :: Int}  -- ^ Regular link
              | Delayed -- ^ automatic delaying

defaultLink :: Monoid lab => Link lab
defaultLink = Link mempty (denselyDotted . outline "black")  0


-------------------

data Rule lab = Rule {ruleStyle :: LineStyle, delimiter :: lab, ruleLabel :: lab, conclusion :: lab}
--  deriving Show

type Premise lab = Link lab ::> Derivation lab
type Derivation lab = Tree (Link lab) (Rule lab)

--------------------------------------------------
-- Delay

depth :: forall lab t. Link lab ::> Tree (Link lab) t -> Int
depth (Link{steps} ::> Node _ ps) = 1 + steps + maximum (0 : map depth ps)

isDelayed :: Premise lab -> Bool
isDelayed (Delayed{} ::> _) = True
isDelayed _ = False

delayPre :: forall lab a. Int -> Link lab ::> a -> Link lab ::> a
delayPre s (Link {..} ::> j) = Link {steps = s, ..} ::> j

delayD :: Monoid lab => Derivation lab -> Derivation lab
delayD (Node r ps0) = Node r (map delayP ps)
    where ps = fmap (fmap delayD) ps0
          ps' = filter (not . isDelayed) ps
          delayP (Delayed ::> d) = defaultLink {steps = 1 + maximum (0 : map depth ps')} ::> d
          delayP p = p

----------------------------------------------------------
-- TeXify
  
-- | Render a derivation tree using simple latex only (links will not be rendered properly)
derivationTree :: Derivation TeX -> TeX
derivationTree = stringizeTex

stringizeTex :: Derivation TeX -> TeX
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

derivationTreeD :: Derivation TeX -> TeX
derivationTreeD d = element $ (derivationTreeDiag $ delayD d :: TexDiagram ())

derivationTreeDiag :: Monad m => Derivation lab -> Diagram lab m ()
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

toDiagPart :: Monad m => Expr -> Premise lab -> Diagram lab m (T.Tree (Point,Anchorage,Point))
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

chainBases :: Monad m => Expr -> [Anchorage] -> Diagram lab m (Anchorage,Expr)
chainBases _ [] = do
  o <- box
  return (o,0)
chainBases spacing ls = do
  grp <- box
  forM_ [Base,N,S] $ \ anch -> do
    D.align ypart $ map (# anch) (grp:ls)
  dxs <- forM (zip ls (tail ls)) $ \(x,y) -> do
    let dx = xdiff (x # E) (y # W)
    dx >== spacing
    return dx
  D.align xpart [grp # W,head ls # W]
  D.align xpart [grp # E,last ls # E]
  return (grp,avg dxs)

-- | Put object in a box of the same vertical extent, and baseline,
-- but whose height can be bigger.
relaxHeight :: (Monad m, Anchored a) => a -> Diagram lab m Anchorage
relaxHeight o = do
  b <- box
  -- using (outline "green")$ traceBounds o
  D.align xpart [b#W,o#W]
  D.align xpart [b#E,o#E]
  D.align ypart [b#Base,o#Base]
  o `fitsVerticallyIn` b
  return b

toDiagram :: Monad m => Expr -> Derivation lab -> Diagram lab m (T.Tree (Point,Anchorage,Point))
toDiagram layerHeight (Node Rule{..} premises) = do
  ps <- mapM (toDiagPart layerHeight) premises
  concl <- relaxHeight =<< extend 1.5 <$> labelBox conclusion
  -- using (outline "red")$ traceBounds concl
  lab <- labelBox ruleLabel

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
  using ruleStyle $ path $ polyline [separ # W,separ # E]
  return $ T.Node (separ # W, concl, lab # E) ps

-----------------------


rule :: Monoid lab => lab -> lab -> Rule lab
rule ruleLabel conclusion = Rule {delimiter = mempty, ruleStyle = outline "black", ..}

dummy :: Monoid lab => Rule lab
dummy = (rule mempty mempty) {ruleStyle = const defaultPathOptions}

emptyDrv :: forall k lab. Monoid lab => Tree k (Rule lab)
emptyDrv = Node dummy []

-- abortDrv (Node Rule {..} _) = Node Rule {ruleStyle = Waved, ..} []

-- | Used when the rest of the derivation is known.
haltDrv' :: forall lab. Monoid lab => lab -> Derivation lab -> Derivation lab
haltDrv' tex (Node r _) = Node r {ruleStyle = noOutline}
     [lnk {steps = 1, label = tex} ::> emptyDrv]
  where lnk :: Link lab
        lnk = defaultLink
-- -- | More compact variant
-- haltDrv :: Monoid lab => lab -> Derivation lab -> Derivation lab
-- haltDrv t (Node r _) = Node r [defaultLink ::> Node dummy {conclusion = cmd "vdots" nil >> cmd "hspace" (tex "2pt") >> t} []]

