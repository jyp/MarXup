module MarXup.Tikz where

-- One can get at the sizes of labels by using these macros:

{-
\gdef\mpxshipout{\shipout\hbox\bgroup
  \setbox0=\hbox\bgroup}
\gdef\stopmpxshipout{\egroup  \dimen0=\ht0 \advance\dimen0\dp0
  \dimen1=\ht0 \dimen2=\dp0
  \setbox0=\hbox\bgroup
    \box0
    \ifnum\dimen0>0 \vrule width1sp height\dimen1 depth\dimen2 
    \else \vrule width1sp height1sp depth0sp\relax
    \fi\egroup
  \ht0=0pt \dp0=0pt \box0 \egroup}
\mpxshipout% line 11 ./t.mp
label 
\stopmpxshipout
-}

-- This program can then extract the sizes out of the generated dvi
import Graphics.DVI
import Control.Monad

scaleDimen :: Dimen -> Float
scaleDimen x = fromIntegral x / 65536 -- in points? to check.

getPg () (Page _ [(_,Box objs)] _) = return (Just ((),dims))
  where ((width,ascent),Rule _ height) = last objs
        dims = (scaleDimen width, scaleDimen height, scaleDimen ascent)

main = do
  pgs <- withDVI "theFile.dvi" (\_ _ -> return emptyFont) () getPg
  forM pgs (putStrLn . show)
  return ()
