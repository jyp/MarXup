{-# OPTIONS_GHC -pgmF marchup -F #-}

import MarchUp.Pandoc
import Text.Pandoc

text = Pandoc (Meta @" A very simple test @" [@"JP Bernardy@"] []) @"

@Header(1){Introduction}


As the sage says:

@BlockQuote{
blah blah arsarst
arsa rsdhwp
}

Do not forget: @Emph{this}!

@Header(1){Conclusion}


@"

main = do
  print text
  putStrLn $ writeMarkdown defaultWriterOptions $ text
