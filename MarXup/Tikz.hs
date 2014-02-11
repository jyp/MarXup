{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards #-}

module MarXup.Tikz (module MarXup.Tikz) where

import Data.String
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Applicative
import MarXup
import MarXup.MultiRef
import MarXup.Tex
import Control.Monad.LPMonad
import qualified Data.Map as M
import Data.Map (Map)
import Data.LinearProgram
import Data.LinearProgram.Common as MarXup.Tikz (VarKind(..)) 
import Data.LinearProgram.LinExpr
import System.IO.Unsafe
import Numeric (showFFloat)


type LPState = LP Var Constant
data Env = Env {diaSolution :: Solution, diaPathOptions :: PathOptions}

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

-- | A point in 2d space
data Point = Point {xpart :: Expr, ypart :: Expr}


-------------
-- Diagrams

runDiagram :: Diagram a -> Multi a
runDiagram (Dia diag) = do
  rec (a,(_,problem),_) <- runRWST diag (Env solution defaultPathOptions)
                                        (Var 0,LP Min M.empty [] M.empty M.empty)
      let solution = case unsafePerformIO $ glpSolveVars simplexDefaults problem of
            (_retcode,Just (_objFunc,s)) -> s
            (retcode,Nothing) -> error $ "ret code = " ++ show retcode
  Raw Normal $ "%" ++ show problem ++ "\n"
  return a

diaRawTex :: Tex a -> Diagram a
diaRawTex (Tex t) = Dia $ lift (runReaderT t ("<in diagra>",EPS))

diaRaw :: String -> Dia
diaRaw = diaRawTex . tex

instance Element (Diagram ()) where
  type Target (Diagram ()) = TeX
  element d = env "tikzpicture" $
      Tex $ lift $ runDiagram d

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
    diaRaw $ showFFloat (Just 4) v tikzUnit
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
minimize (LinExpr x _) = addObjective x
maximize = minimize . negate



----------------
-- Points
instance Num Point where
  Point x1 y1 + Point x2 y2 = Point (x1 + x2) (y1 + y2)
  negate (Point x y) = Point (negate x) (negate y)

--------------------
-- Point rendering
instance Element Point where
  type Target Point = Diagram ()
  element (Point x y) = "(" <> element x <> "," <> element y <> ")"

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
---------------------
-- Point objectives

southwards, northwards, westwards, eastwards :: Point -> Diagram ()
southwards (Point _ y) = minimize y
westwards (Point x _) = minimize x
northwards = southwards . negate
eastwards = westwards . negate

-----------------
-- Paths

data Path = EmptyPath |
            Path {startingPoint :: Point
                 ,segments :: [Segment]}

data Segment = StraightTo Point | CurveTo Point Point Point | HV Point | VH Point | Cycle | Rounded (Maybe Constant)

instance Element Segment where
  type Target Segment = Diagram ()
  element (StraightTo p) = "--" <> element p
  element (VH p) = "|-" <> element p
  element (HV p) = "-|" <> element p
  element (CurveTo c d p) = "..controls" <> element c <> "and" <> element d <> ".." <> element p
  element Cycle = "--cycle"
  element (Rounded Nothing) = "[sharp corners]"
  element (Rounded (Just r)) = "[" <> element (constant r) <> "]"

instance Element Path where
  type Target Path = Diagram ()
  element = path

path :: Path -> Dia
path EmptyPath = return ()
path (Path start segs) = do
  options <- diaPathOptions <$> ask
  "\\path"
    <> element options
    <> element start <> mapM_ element segs
      <> ";\n"

polyline :: [Point] -> Path
polyline [] = EmptyPath
polyline (x:xs) = Path x (map StraightTo xs)

polygon :: [Point] -> Path
polygon [] = EmptyPath
polygon (x:xs) = Path x (map StraightTo xs ++ [Cycle])



-----------------
-- Path Options

localPathOptions :: (PathOptions -> PathOptions) -> Diagram a -> Diagram a
localPathOptions f = local $ \(Env s o) -> Env s (f o)

draw :: Path -> Dia
draw p = localPathOptions (\o -> o {drawColor = Just "black"}) $ path p

data LineTip = Circle | None | Stealth | Latex | Reversed LineTip | Bracket | Parens
instance Show LineTip where
  show t = case t of
    Circle -> "o"
    None -> ""
    Latex -> "latex"
    Reversed x -> show x ++ " reversed"
    Bracket -> "["
    Parens -> "("
      
type Color = String
data LineCap = Butt | Rect | RoundCap
data LineJoin = Miter | RoundJoin | Bevel
type DashPattern = [Constant] -- On x1, off x2, ...

ultraThin, veryThin, thin, semiThick, thick, veryThick, ultraThick :: Constant
ultraThin = 0.1
veryThin = 0.2
thin = 0.4 -- def
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
  {drawColor = Nothing
  ,fillColor = Nothing
  ,lineWidth = thin
  ,startTip  = None
  ,endTip    = None
  ,lineCap   = Butt
  ,lineJoin  = Miter
  ,dashPattern = solidDash
  }

data PathOptions = PathOptions
                     {drawColor :: Maybe Color
                     ,fillColor :: Maybe Color
                     ,lineWidth :: Constant
                     ,startTip  :: LineTip
                     ,endTip    :: LineTip
                     ,lineCap   :: LineCap
                     ,lineJoin  :: LineJoin
                     ,dashPattern :: DashPattern
                     }

instance Element PathOptions where
  type Target PathOptions = Diagram ()
  element PathOptions{..} = "["
    <> diaRaw (show startTip ++ "-" ++ show endTip) <> ","
    <> col "draw" drawColor
    <> col "fill" drawColor
    <> "line width=" <> element (constant lineWidth) <> ","
    -- <> "line cap=" <>  <> ","
    -- <> "line join=" <>  <> ","
    -- <> "dash pattern=" <>  <> ","
    <> "]"
    where col attr = maybe "" (\c -> attr <> "=" <> diaRaw c <> ",")

----------
-- Text

drawText :: Point -> TeX -> Diagram BoxSpec
drawText point t = do
  diaRawTex $ tex $ "\\node[anchor=north west,inner sep=0] at "
  element point
  (_,box) <- diaRawTex $ inBox $ braces $ t
  diaRawTex $ tex ";\n"
  return box
