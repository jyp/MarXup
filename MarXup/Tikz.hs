{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo #-}

module MarXup.Tikz where

import Control.Monad.RWS
import Control.Monad.Reader
import Control.Applicative
import MarXup.MultiRef
import MarXup.Tex
import Control.Monad.LPMonad
import qualified Data.Map as M
import Data.Map (Map)
import Data.LinearProgram
import Data.LinearProgram.LinExpr
import Control.Arrow (first,second)
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

runDiagram :: Diagram a -> Multi a
runDiagram (Dia diag) = do
  rec (a,(_,problem),_) <- runRWST diag solution (Var 0,LP Min M.empty [] M.empty M.empty)
      let solution = case unsafePerformIO $ glpSolveVars simplexDefaults problem of
            (_retcode,Just (_objFunc,s)) -> s
            (retcode,Nothing) -> error $  "ret code = " ++ show retcode
  return a

embed :: Var -> Expr
embed v = LinExpr (var v) 0

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

newVars :: [VarKind] -> Diagram [Var]
newVars kinds = forM kinds $ \k -> do
  v <- rawNewVar
  setVarKind v k
  return v


diaRawTex :: Tex a -> Diagram a
diaRawTex (Tex t) = Dia $ lift (runReaderT t ("<in diagra>",EPS))

drawText :: Point -> TeX -> Diagram BoxSpec
drawText (Point x y) t = do
  x' <- valueOf x
  y' <- valueOf y
  diaRawTex $ tex $ "\\path (" ++ show x' ++ "," ++ show y' ++ ")"
  (_,box) <- diaRawTex $ inBox $ braces $ t
  diaRawTex $ tex ";"
  return box
