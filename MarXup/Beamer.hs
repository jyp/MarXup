{-# LANGUAGE OverloadedStrings #-}
module MarXup.Beamer where

import Control.Applicative
import Data.Monoid
import MarXup.Tex
import MarXup.Latex (itemize,enumerate,item,color)
import Data.List (intercalate)

usetheme = cmd "usetheme" . tex

frame tit bod = env "frame" $ do
  cmd "frametitle" tit
  bod

framesubtitle = cmd "framesubtitle"
itemiz xs = itemize $ mconcat $ fmap (item <>) xs
enumerat xs = enumerate $ mconcat $ fmap (item <>) xs

note = cmd "note"

hide = color "white"

-----------------------------------
-- Discouraged commands
-- (it's usually better to generate the various frames using Haskell code)
pause :: TeX
pause = cmd0 "pause"

frameCmd :: String -> [Int] -> TeX -> TeX
frameCmd cmd frames body = do
  tex $ "\\ " ++ cmd ++ "<"
  tex $ intercalate "," $ map show frames
  tex ">"
  braces body

only :: [Int] -> TeX -> TeX
only = frameCmd "only"
uncover :: [Int] -> TeX -> TeX
uncover = frameCmd "uncover"
