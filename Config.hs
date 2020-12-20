{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.Configurator
import Data.Configurator.Types (Value (..),Configured (..))
import System.IO.Unsafe

data List a = L [a]

instance Configured a => Configured (List a) where
   convert (List vs) = L <$> traverse convert vs
   convert _ = Nothing

antiQuoteStrings :: [String]
quoteStrings :: [(String,String)]

(antiQuoteStrings,quoteStrings) = unsafePerformIO $ do
    cfg <- load [Optional ".marxup"]

    L aq <- lookupDefault (L ["@"]) cfg "antiQuotes"
    L q  <- lookupDefault (L [("«","»")]) cfg "quotes"

    return (aq,q)


-- Local Variables:
-- dante-target: "marxup"
-- End:
