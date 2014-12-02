{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies,TypeSynonymInstances,FlexibleInstances, PackageImports, DeriveFunctor #-}

module MarXup.Verbatim where

import MarXup
import Control.Monad.Fix
import Control.Monad (ap)
import Control.Applicative

data Verbatim a = Verbatim {fromVerbatim::String, value::a}
  deriving Functor

instance Textual Verbatim where
    textual s = Verbatim s ()

instance Applicative Verbatim where
    pure = return
    (<*>) = ap

instance Monad Verbatim where
    return x = Verbatim "" x
    (Verbatim s0 x) >>= f =
        Verbatim (s0 ++ s1) y
        where Verbatim s1 y = f x

instance MonadFix Verbatim where
    mfix f = let Verbatim _ x = f x in f x

