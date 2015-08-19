{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell #-}

module MarXup.Diagram.Layout (module MarXup.Diagram.Layout) where
import Control.Monad.LPMonad
import Prelude hiding (sum,mapM_,mapM,concatMap)
import Control.Monad.RWS hiding (forM,forM_,mapM_,mapM)
import Data.LinearProgram
import Data.LinearProgram.Common as MarXup.Diagram.Layout (VarKind(..)) 
import Data.LinearProgram.LinExpr
import Data.Map (Map)
import qualified Data.Map as M
import Control.Lens hiding (element)
import Data.String
-- import MarXup
import Data.Traversable
import Data.Foldable
import Control.Applicative
import System.IO.Unsafe
import MarXup.MultiRef
import MarXup.Tex

type LPState = LP Var Constant

-- | Solution of the linear programming problem
type Solution = Map Var Double


type Constant = Double

-- | Expressions are linear functions of the variables
type Expr = LinExpr Var Constant

-- | Tikz decoration
newtype Decoration = Decoration String


-- | Tikz line tip
data LineTip = ToTip | CircleTip | NoTip | StealthTip | LatexTip | ReversedTip LineTip | BracketTip | ParensTip

-- | Tikz color
type Color = String

-- | Tikz line cap
data LineCap = ButtCap | RectCap | RoundCap

-- | Tikz line join
data LineJoin = MiterJoin | RoundJoin | BevelJoin

-- | Tikz dash pattern
type DashPattern = [(Constant,Constant)]

-- | Path drawing options
data PathOptions = PathOptions
                     {_drawColor :: Maybe Color
                     ,_fillColor :: Maybe Color
                     ,_lineWidth :: Constant
                     ,_startTip  :: LineTip
                     ,_endTip    :: LineTip
                     ,_lineCap   :: LineCap
                     ,_lineJoin  :: LineJoin
                     ,_dashPattern :: DashPattern
                     ,_decoration :: Decoration
                     }
$(makeLenses ''PathOptions)

data Env = Env {_diaSolution :: Solution
               ,_diaTightness :: Constant -- ^ Multiplicator to minimize constraints
               ,_diaPathOptions :: PathOptions}

$(makeLenses ''Env)

defaultPathOptions :: PathOptions
defaultPathOptions = PathOptions
  {_drawColor = Nothing
  ,_fillColor = Nothing
  ,_lineWidth = 0.4
  ,_startTip  = NoTip
  ,_endTip    = NoTip
  ,_lineCap   = ButtCap
  ,_lineJoin  = MiterJoin
  ,_dashPattern = []
  ,_decoration = Decoration ""
  }

newtype Diagram a = Dia (RWST Env () (Var,LPState) (Multi ClassFile Key) a)
  deriving (Monad, Applicative, Functor, MonadReader Env)

type Dia = Diagram ()

instance MonadState LPState Diagram where
  get = Dia $ snd <$> get
  put y = Dia $ do
    (x,_) <- get
    put (x,y)

-------------
-- Diagrams

runDiagram :: Diagram a -> Multi ClassFile Key a
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

relax :: Constant -> Diagram a -> Diagram a
relax factor = local (over diaTightness (/ factor))

instance Monoid (Diagram ()) where
  mempty = return ()
  mappend = (>>)

instance IsString (Diagram ()) where
  fromString = diaRawTex . tex

--------------
-- Variables
varValue :: Var -> Diagram Double
varValue v = M.findWithDefault 0 v <$> view diaSolution

rawNewVar :: Diagram Var
rawNewVar = Dia $ do
      (Var x,y) <- get
      put $ (Var (x+1),y)
      return $ Var x

newVar :: Diagram Expr
newVar = do
  [v] <- newVars [ContVar]
  return v

newVars :: [VarKind] -> Diagram [Expr]
newVars kinds = newVars' (zip kinds (repeat Free))

newVars' :: [(VarKind,Bounds Constant)] -> Diagram [Expr]
newVars' kinds = forM kinds $ \(k,b) -> do
  v <- rawNewVar
  setVarKind v k
  setVarBounds v b
  return $ variable v

infix 4 <==,===,>==

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

-- | Minimum or maximum of a list of expressions.
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
  tightness <- view diaTightness
  addObjective (tightness *- x)
maximize = minimize . negate


