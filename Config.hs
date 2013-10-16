{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator
import Data.Configurator.Types (Value (..),Configured (..))
import System.IO.Unsafe
import Data.Traversable
import Control.Applicative

data List a = L [a]

instance Configured a => Configured (List a) where
   convert (List vs) = L <$> traverse convert vs
   convert _ = Nothing

antiQuoteStrings :: [String]
quotesStrings :: [(String,String)]

(antiQuoteStrings,quotesStrings) = unsafePerformIO $ do
    cfg <- load [Optional ".marxup"]

    L aq <- lookupDefault (L ["@"]) cfg "antiQuotes"
    L q  <- lookupDefault (L [("«","»")]) cfg "quotes"

    return (aq,q)
