{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies #-}

module MarXup.Tikz where

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
import Data.LinearProgram.LinExpr
import System.IO.Unsafe


type LPState = LP Var Constant

newtype Diagram a = Dia (RWST Solution () (Var,LPState) Multi a)
  deriving (Monad, Applicative, Functor, MonadReader Solution)

instance MonadState LPState Diagram where
  get = Dia $ snd <$> get
  put y = Dia $ do
    (x,_) <- get
    put (x,y)
  
type Solution = Map Var Double
type Constant = Double
type Expr = LinExpr Var Constant
data Point = Point {xpart :: Expr, ypart :: Expr}

instance Element (Diagram ()) where
  type Target (Diagram ()) = TeX
  element d = env "tikzpicture" $
      Tex $ lift $ runDiagram d

instance Monoid (Diagram ()) where
  mempty = return ()
  mappend = (>>)

tikzUnit :: String
tikzUnit = "pt"

instance Element Point where
  type Target Point = Diagram ()
  element (Point x y) = do
     x' <- valueOf x
     y' <- valueOf y
     diaRawTex $ tex $ "(" ++ show x' ++ tikzUnit ++ "," ++ show y' ++ tikzUnit ++ ")"

instance IsString (Diagram ()) where
  fromString = diaRawTex . tex

runDiagram :: Diagram a -> Multi a
runDiagram (Dia diag) = do
  rec (a,(_,problem),_) <- runRWST diag solution (Var 0,LP Min M.empty [] M.empty M.empty)
      let solution = case unsafePerformIO $ glpSolveVars simplexDefaults problem of
            (_retcode,Just (_objFunc,s)) -> s
            (retcode,Nothing) -> error $ "ret code = " ++ show retcode
  Raw Normal $ "%" ++ show problem ++ "\n"
  return a

variable :: Var -> Expr
variable v = LinExpr (var v) 0

constant :: Constant -> Expr
constant c = LinExpr M.empty c

instance Num Expr where
  fromInteger x = LinExpr M.empty (fromInteger x)
  negate = neg
  (+) = (^+^)
  (-) = (^-^)

instance Num Point where
  Point x1 y1 + Point x2 y2 = Point (x1 + x2) (y1 + y2)
  negate (Point x y) = Point (negate x) (negate y)

askSol :: Diagram Solution
askSol = ask

varValue :: Var -> Diagram Double
varValue v = M.findWithDefault 0 v <$> askSol

valueOf :: Expr -> Diagram Double
valueOf (LinExpr m c) = do
  vs <- forM (M.assocs m) $ \(v,scale) ->
    (scale *) <$> varValue v
  return $ sum $ c:vs

rawNewVar :: Diagram Var
rawNewVar = Dia $ do
      (Var x,y) <- get
      put $ (Var (x+1),y)
      return $ Var x

newVars :: [VarKind] -> Diagram [Expr]
newVars kinds = forM kinds $ \k -> do
  v <- rawNewVar
  setVarKind v k
  return $ variable v


diaRawTex :: Tex a -> Diagram a
diaRawTex (Tex t) = Dia $ lift (runReaderT t ("<in diagra>",EPS))

diaRaw = diaRawTex . tex

drawText :: Point -> TeX -> Diagram BoxSpec
drawText point t = do
  diaRawTex $ tex $ "\\node[anchor=north west,inner sep=0] at "
  element point
  (_,box) <- diaRawTex $ inBox $ braces $ t
  diaRawTex $ tex ";\n"
  return box



(===) :: Expr -> Expr -> Diagram ()
e1 === e2 = do
  let LinExpr f c = e1 - e2
  equalTo f (negate c)

(.=.) :: Point -> Point -> Diagram ()
Point x1 y1 .=. Point x2 y2 = do
  x1 === x2
  y1 === y2

-- scale :: Constant -> Expr -> Expr
-- scale sc (LinExpr f c) = 
avg :: [Expr] -> Expr
avg xs = (1/fromIntegral (length xs) :: Double) *^ sum xs
