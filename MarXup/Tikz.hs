{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell #-}

module MarXup.Tikz (module MarXup.Tikz) where

import Control.Lens hiding (element)
import Prelude hiding (sum,mapM_,mapM,concatMap)
import qualified Geom2D.CubicBezier as CB
import Geom2D.CubicBezier (CubicBezier(..))
import Control.Applicative
import Control.Monad.LPMonad
import "mtl" Control.Monad.RWS hiding (forM,forM_,mapM_,mapM)
-- import "mtl" Control.Monad.Reader hiding (forM,forM_,mapM_,mapM)
import Data.LinearProgram
import Data.LinearProgram.Common as MarXup.Tikz (VarKind(..)) 
import Data.LinearProgram.LinExpr
import Data.List (sort,transpose)
import Data.Map (Map)
import Data.Maybe (listToMaybe)
import Data.String
import MarXup
import MarXup.MultiRef
import MarXup.Tex
import Numeric (showFFloat)
import System.IO.Unsafe
import qualified Data.Map as M
import Data.Traversable
import Data.Foldable

type LPState = LP Var Constant
data Env = Env {diaSolution :: Solution
               ,diaTightness :: Constant -- ^ Multiplicator to minimize constraints
               ,diaPathOptions :: PathOptions}

type Dia = Diagram ()

newtype Diagram a = Dia (RWST Env () (Var,LPState) Multi a)
  deriving (Monad, Applicative, Functor, MonadReader Env)

instance MonadState LPState Diagram where
  get = Dia $ snd <$> get
  put y = Dia $ do
    (x,_) <- get
    put (x,y)

type Solution = Map Var Double
type Constant = Double

-- | Expressions are linear functions of the variables
type Expr = LinExpr Var Constant

-------------
-- Diagrams

runDiagram :: Diagram a -> Multi a
runDiagram (Dia diag) = do
  rec (a,(_,problem),_) <- runRWST diag (Env solution 1 defaultPathOptions)
                                        (Var 0,LP Min M.empty [] M.empty M.empty)
      let solution = case unsafePerformIO $ glpSolveVars simplexDefaults problem of
            (_retcode,Just (_objFunc,s)) -> s
            (retcode,Nothing) -> error $ "ret code = " ++ show retcode
  -- Raw Normal $ "%problem solved: " ++ show problem ++ "\n"
  return a

diaRawTex :: Tex a -> Diagram a
diaRawTex (Tex t) = Dia $ lift t

diaRaw :: String -> Dia
diaRaw = diaRawTex . tex

instance Element (Diagram ()) where
  type Target (Diagram ()) = TeX
  element d = braces $ do
    cmd0 "normalsize"
      -- otherwise the boxes use "normalsize", while tikz inherits
      -- the smaller or bigger size from the current scope. Actually,
      -- every text styling should be reset, but I don't know how to
      -- do that.
    env "tikzpicture" $
      Tex $ runDiagram d

instance Monoid (Diagram ()) where
  mempty = return ()
  mappend = (>>)

instance IsString (Diagram ()) where
  fromString = diaRawTex . tex


--------------
-- Variables
varValue :: Var -> Diagram Double
varValue v = M.findWithDefault 0 v . diaSolution <$> ask

rawNewVar :: Diagram Var
rawNewVar = Dia $ do
      (Var x,y) <- get
      put $ (Var (x+1),y)
      return $ Var x

newVars :: [VarKind] -> Diagram [Expr]
newVars kinds = newVars' (zip kinds (repeat Free))

newVars' :: [(VarKind,Bounds Constant)] -> Diagram [Expr]
newVars' kinds = forM kinds $ \(k,b) -> do
  v <- rawNewVar
  setVarKind v k
  setVarBounds v b
  return $ variable v

infix 4 .=.,<==,===,>==

----------------
-- Expressions
instance Fractional Expr where
  fromRational ratio = constant (fromRational ratio)
    
instance Num Expr where
  fromInteger x = LinExpr M.empty (fromInteger x)
  negate = neg
  (+) = (^+^)
  (-) = (^-^)

valueOf :: Expr -> Diagram Double
valueOf (LinExpr m c) = do
  vs <- forM (M.assocs m) $ \(v,scale) ->
    (scale *) <$> varValue v
  return $ sum $ c:vs

instance Element Expr where
  type Target Expr = Dia
  element x = do
    v <- valueOf x
    diaRaw $ showDistance v

showDistance :: Constant -> String
showDistance x = showFFloat (Just 4) x tikzUnit
    where tikzUnit = "pt"

variable :: Var -> Expr
variable v = LinExpr (var v) 0

constant :: Constant -> Expr
constant c = LinExpr M.empty c

(*-) :: Module Constant a => Constant -> a -> a
(*-) = (*^)
infixr 6 *-

avg :: Module Constant a => [a] -> a
avg xs = (1/fromIntegral (length xs)) *- gsum xs

absoluteValue :: Expr -> Diagram Expr
absoluteValue x = do
  [t1,t2] <- newVars' [(ContVar,LBound 0),(ContVar,LBound 0)]
  t1 - t2 === x
  return $ t1 + t2

satAll :: (Expr -> a -> Diagram b) -> [a] -> Diagram Expr
satAll p xs = do
  [m] <- newVars [ContVar]
  mapM_ (p m) xs
  return m

maximVar, minimVar :: [Expr] -> Diagram Expr
maximVar = satAll (>==)
minimVar = satAll (<==)

--------------
-- Expression constraints
(>==), (<==) :: Expr -> Expr -> Diagram ()
e1 <== e2 = do
  let LinExpr f c = e1 - e2
  leqTo f (negate c)

(>==) = flip (<==)

(===) :: Expr -> Expr -> Diagram ()
e1 === e2 = do
  let LinExpr f c = e1 - e2
  equalTo f (negate c)

-- | minimize the distance between expressions
(=~=) :: Expr -> Expr -> Diagram ()
x =~= y = minimize =<< absoluteValue (x-y)

-------------------------
-- Expression objectives

minimize,maximize :: Expr -> Diagram ()
minimize (LinExpr x _) = do
  tightness <- diaTightness <$> ask
  addObjective (tightness *- x)
maximize = minimize . negate



----------------
-- Points
-- | A point in 2d space
data Point = Point {xpart :: Expr, ypart :: Expr}
  deriving (Eq,Show)
           
instance Num Point where
  negate = neg
  (+) = (^+^)
  (-) = (^-^)

instance Group Point where
  zero = Point zero zero
  Point x1 y1 ^+^ Point x2 y2 = Point (x1 ^+^ x2) (y1 ^+^ y2)
  neg (Point x y) = Point (neg x) (neg y)

instance Module Constant Point where
  k *^ Point x y = Point (k *^ x) (k *^ y)

--------------------
-- Point rendering
instance Element Point where
  type Target Point = Diagram ()
  element (Point x y) = "(" <> element x <> "," <> element y <> ")"

instance Element CB.Point where
  type Target CB.Point = String
  element (CB.Point x y) = "(" <> showDistance x <> "," <> showDistance y <> ")"

-----------------
-- Point constraints

(.=.),northOf,southOf,westOf,eastOf :: Point -> Point -> Diagram ()
Point x1 y1 .=. Point x2 y2 = do
  x1 === x2
  y1 === y2

northOf (Point _ y1) (Point _ y2) = y2 <== y1
southOf = flip northOf
westOf (Point x1 _) (Point x2 _) = x1 <== x2
eastOf = flip westOf

alignHoriz,alignVert :: [Point] -> Diagram ()
alignHoriz = align ypart
alignVert = align xpart

align :: (a -> Expr) -> [a] -> Diagram ()
align _ [] = return ()
align f (p:ps) = forM_ ps $ \p' -> f p === f p'

alignMatrix :: [[Point]] -> Dia
alignMatrix ls = do
  forM_ ls alignHoriz
  forM_ (transpose ls) alignVert

---------------------
-- Point objectives

southwards, northwards, westwards, eastwards :: Point -> Diagram ()
southwards (Point _ y) = minimize y
westwards (Point x _) = minimize x
northwards = southwards . negate
eastwards = westwards . negate

-----------------
-- Paths

type Path = Path' Point

data Path' a
  = EmptyPath
  | Path {startingPoint :: a
         ,segments :: [Segment a]}
  deriving Show
           
instance Functor Path' where
  fmap = fmapDefault

instance Foldable Path' where
  foldMap = foldMapDefault
instance Traversable Path' where
  traverse _ EmptyPath = pure EmptyPath
  traverse f (Path s ss) = Path <$> f s <*> traverse (traverse f) ss

freezePoint :: Point -> Diagram CB.Point
freezePoint (Point x y) = CB.Point <$> valueOf x <*> valueOf y

freezePath :: Path' Point -> Diagram (Path' CB.Point)
freezePath = traverse freezePoint

-- toBeziers :: PathPoint -> [CubicBezier]
toBeziers EmptyPath = []
toBeziers (Path start ss) | not (null ss) &&
                            isCycle (last ss) = toBeziers' start (init ss ++ [StraightTo start])
                          | otherwise = toBeziers' start ss
fromBeziers [] = EmptyPath
fromBeziers (CubicBezier p c d q:bs) = Path p (CurveTo c d q:pathSegments (fromBeziers bs))

pathSegments EmptyPath = []
pathSegments (Path _ ss) = ss

isCycle Cycle = True
isCycle _  = False

instance Group CB.Point where
  zero = CB.Point 0 0
  (^+^) = (CB.^+^)
  (^-^) = (CB.^-^)
  
instance Module Constant CB.Point where
  (*^) = (CB.*^)


diaDebug msg = diaRaw $ "\n%DBG:" ++ msg ++ "\n"

toBeziers' :: CB.Point -> [Segment CB.Point] -> [CubicBezier]
toBeziers' _ [] = []
toBeziers' start (StraightTo next:ss) = CubicBezier start mid mid next : toBeziers' next ss
  where mid = avg [start, next]
toBeziers' p (CurveTo c d q:ss) = CubicBezier p c d q : toBeziers' q ss

clipOne :: CubicBezier -> [CubicBezier] -> Maybe CubicBezier
clipOne b cutter = fmap firstPart $ listToMaybe $ sort $ concatMap (\b' -> map fst $ CB.bezierIntersection b b' 0.001) cutter
  where firstPart t = fst $ CB.splitBezier b t

-- | @cutAfter path area@ cuts the path after its first intersection with the @area@.
cutAfter' :: [CubicBezier] -> [CubicBezier] -> [CubicBezier]
cutAfter' [] _cutter = []
cutAfter' (b:bs) cutter = case clipOne b cutter of
  Nothing -> b:cutAfter' bs cutter
  Just b' -> [b']

revBeziers :: [CubicBezier] -> [CubicBezier]
revBeziers = reverse . map rev
  where rev (CubicBezier a b c d) = CubicBezier d c b a
        
cutBefore' path area = revBeziers $ cutAfter' (revBeziers path) area

onBeziers op p' q' = fromBeziers $ op (toBeziers p') (toBeziers q')

type FrozenPath = Path' CB.Point

cutAfter :: FrozenPath -> FrozenPath -> FrozenPath
cutAfter = onBeziers cutAfter'

cutBefore :: FrozenPath -> FrozenPath -> FrozenPath
cutBefore = onBeziers cutBefore'

data Segment point = CurveTo point point point
                   | StraightTo point
                   | Cycle
                   -- | Rounded (Maybe Constant)
                   -- | HV point | VH point
  deriving (Show,Eq)
instance Functor Segment where
  fmap = fmapDefault
  
instance Foldable Segment where
  foldMap = foldMapDefault
instance Traversable Segment where
  traverse _ Cycle = pure Cycle
  traverse f (StraightTo p) = StraightTo <$> f p
  traverse f (CurveTo c d q) = CurveTo <$> f c <*> f d <*> f q
  
instance (Element point,Monoid (Target point), IsString (Target point)) => Element (Segment point) where
  type Target (Segment point) = Target point
  element (StraightTo p) = "--" <> element p
  element (CurveTo c d p) = "..controls" <> element c <> "and" <> element d <> ".." <> element p
  element Cycle = "--cycle"
  -- element (VH p) = "|-" <> element p
  -- element (HV p) = "-|" <> element p
  -- element (Rounded Nothing) = "[sharp corners]"
  -- element (Rounded (Just r)) = "[" <> element (constant r) <> "]"

instance Element Path where
  type Target Path = Diagram ()
  element = path

path :: Path -> Dia
path = frozenPath <=< freezePath

frozenPath :: Path' CB.Point  -> Dia
frozenPath p  = do
  options <- diaPathOptions <$> ask
  diaRaw $ "\\path"
    <> element options
    <> case p of
      EmptyPath -> ""
      (Path start segs) -> element start ++ concatMap element segs
  diaRaw ";\n"

polyline :: [Point] -> Path
polyline [] = EmptyPath
polyline (x:xs) = Path x (map StraightTo xs)

polygon :: [Point] -> Path
polygon [] = EmptyPath
polygon (x:xs) = Path x (map StraightTo xs ++ [Cycle])



-----------------
-- Path Options

localPathOptions :: (PathOptions -> PathOptions) -> Diagram a -> Diagram a
localPathOptions f = local $ \e -> e {diaPathOptions = f (diaPathOptions e)}

data LineTip = ToTip | CircleTip | NoTip | StealthTip | LatexTip | ReversedTip LineTip | BracketTip | ParensTip
instance Show LineTip where
  show t = case t of
    ToTip -> "to"
    CircleTip -> "o"
    NoTip -> ""
    LatexTip -> "latex"
    ReversedTip x -> show x ++ " reversed"
    BracketTip -> "["
    ParensTip -> "("

type Color = String
data LineCap = ButtCap | RectCap | RoundCap
data LineJoin = MiterJoin | RoundJoin | BevelJoin
type DashPattern = [Constant] -- On x1, off x2, ...

ultraThin, veryThin, thin, semiThick, thick, veryThick, ultraThick :: Constant
ultraThin = 0.1
veryThin = 0.2
thin = 0.4
semiThick = 0.6
thick = 0.8
veryThick = 1.2
ultraThick = 1.6

solidDash :: DashPattern
solidDash = [1]

showDashPat :: Bool -> DashPattern -> String
showDashPat _ [] = ""
showDashPat on (x:xs) = (if on then "on" else "off") <> " " <> show x <>
                        " " <> showDashPat (not on) xs

defaultPathOptions :: PathOptions
defaultPathOptions = PathOptions
  {_drawColor = Nothing
  ,_fillColor = Nothing
  ,_lineWidth = thin
  ,_startTip  = NoTip
  ,_endTip    = NoTip
  ,_lineCap   = ButtCap
  ,_lineJoin  = MiterJoin
  ,_dashPattern = solidDash
  }

data PathOptions = PathOptions
                     {_drawColor :: Maybe Color
                     ,_fillColor :: Maybe Color
                     ,_lineWidth :: Constant
                     ,_startTip  :: LineTip
                     ,_endTip    :: LineTip
                     ,_lineCap   :: LineCap
                     ,_lineJoin  :: LineJoin
                     ,_dashPattern :: DashPattern
                     }
$(makeLenses ''PathOptions)

draw :: Diagram a -> Diagram a
draw = localPathOptions (set drawColor (Just "black"))

instance Element PathOptions where
  type Target PathOptions = String
  element PathOptions{..} = "["
    <> show _startTip <> "-" <> show _endTip <> ","
    <> col "draw" _drawColor
    <> col "fill" _drawColor
    <> "line width=" <> showDistance _lineWidth <> ","
    <> "line cap=" <> (case _lineCap of
                          RoundCap -> "round"
                          RectCap -> "rect"
                          ButtCap -> "butt") <> ","
    <> "line join=" <> (case _lineJoin of
                          RoundJoin -> "round"
                          BevelJoin -> "bevel"
                          MiterJoin -> "miter") <> ","
    <> "dash pattern=" <> showDashPat True _dashPattern
    <> "]"
    where col attr = maybe "" (\c -> attr <> "=" <> c <> ",")

----------
-- Text

drawText :: Point -> TeX -> Diagram BoxSpec
drawText point t = do
  diaRawTex $ tex $ "\\node[anchor=north west,inner sep=0] at "
  element point
  (_,box) <- diaRawTex $ inBox $ braces $ t
  diaRawTex $ tex ";\n"
  return box
