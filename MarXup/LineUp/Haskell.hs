{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module MarXup.LineUp.Haskell where

import Data.List
import Data.Function
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser (ParseResult(..),ParseMode(..),defaultParseMode)
import Language.Haskell.Exts.SrcLoc
import MarXup
import MarXup.LineUp
import MarXup.Tex
import MarXup.Latex.Math
import MarXup.Verbatim
import Data.Char (isDigit)

instance Functor Loc where
  fmap f (Loc l x) = Loc l (f x)

haskell :: Verbatim a -> Tex ()
haskell = haskellCust defaultParseMode (fmap (fmap printTok))

haskellInline :: Verbatim a -> Tex ()
haskellInline = haskellInlineCust defaultParseMode (fmap (fmap printTok))

type ProcessToks = [Loc Token] -> [Loc (Float,TeX,Float)]
type PrintTok = Token -> (Float,TeX,Float)


mkTok :: (Loc (Float, TeX, Float)) -> Tok
mkTok (Loc l (before,txt,after)) = Tok (srcSpanStartColumn l) (srcSpanEndColumn l) before txt after


-- We replace with spaces so that the column numbers are not affected
replaceWithSpaces :: String -> String -> (String -> String) -> String
replaceWithSpaces s xs k  = help (length s) xs k where
  help :: Int -> String -> (String -> String) -> String
  help n xs k  = replicate n ' ' ++ k (drop n xs)

preprocess :: String -> String
preprocess = \case xs | hideSequence `isPrefixOf` xs -> replaceWithSpaces hideSequence xs dropper
                   (x:xs) -> x:preprocess xs
                   [] -> []
  where
    hideSequence = "{-<-}"
    showSequence = "{->-}"
    dropper :: String -> String
    dropper xs     | showSequence `isPrefixOf` xs = replaceWithSpaces showSequence xs preprocess
    dropper ('\n':xs) = '\n':dropper xs
    dropper (_:xs) = ' ':dropper xs
    dropper [] = []

haskellCust :: ParseMode -> ProcessToks -> Verbatim a -> Tex ()
haskellCust mode processToks v = case lexTokenStreamWithMode mode (preprocess $ fromVerbatim v) of
  ParseOk toks -> lineup (map (map mkTok) lins)
    where lins = groupBy ((==) `on` (srcSpanStartLine . loc)) (processToks toks)
  ParseFailed location err -> textual (show location ++ show err)

haskellInlineCust :: ParseMode -> ProcessToks -> Verbatim a -> Tex ()
haskellInlineCust mode processToks v = mconcat (haskellInlineCust' mode processToks v) 

haskellInlineCust' :: ParseMode -> ProcessToks -> Verbatim a -> [TeX]
haskellInlineCust' mode processToks v = case lexTokenStreamWithMode mode (preprocess $ fromVerbatim v) of
   ParseOk toks -> map render $ mkSpaces $ map mkTok  $ processToks toks
   ParseFailed location err -> [textual (show location ++ show err)]


splitTok :: String -> (String, Maybe String)
splitTok input = (reverse rev3 ++ primes, if null subscript then Nothing else Just (reverse subscript))
  where (explicitSubscript,rev3) = case break (== '_') rev2 of
          (everything,"") -> ("",everything)
          (suff,'_':pref) -> (suff,pref)
        (numbers,rev2) = span isDigit rev1
        (primes,rev1) = span (== '\'') rev0
        rev0 = reverse input
        subscript = explicitSubscript ++ numbers

printTok :: PrintTok
printTok t = let self = textual $ showToken t
                 ident = word $ case splitTok $ showToken t of
                              (_,Nothing) -> mathsf self
                              (pref,Just suff) -> mathsf (textual pref) <> tex "_" <> braces (textual suff)
                 unquote = word $ mathsf self
                 quote = word $ mathtt self
                 literal = word $ mathrm self
                 string = word $ mathtt self
                 keyword = word $ mathbf self
                 pragma = word $ mathrm self
                 symbol = word $ mathnormal self
                 leftParen  = (3,allowbreak <> mathnormal self,0)
                 rightParen = (0,mathnormal self <> allowbreak,3)
                 rightParenMed = (0,mathnormal self,4)
                 special x = med $ mathnormal $ tex x
                 debug = thick $ textual "[" <> mathnormal (textual $ show t) <> textual "]"
                 thick s = (5,s,5)
                 verythick s = (6,s,6)
                 med s = (4,s,4)
                 thin s = (3,s,3)
                 word = thin
  in case t of
        -- _ -> mathrm $ textual $ show t -- Debug
        VarId _ -> ident
        QVarId _ -> ident
        IDupVarId _ -> ident
        ILinVarId _ -> ident
        ConId _ -> ident
        QConId _ -> ident
        DVarId _ -> ident
        VarSym "==" -> special "\\equiv" -- ≡
        VarSym "=~" -> special "\\cong" -- ≅
        VarSym "<=" -> special "\\leq" -- ≤
        VarSym ">=" -> special "\\geq" -- ≥
        VarSym "<>" -> special "<\\!>"
        VarSym "<|>" -> special "<\\!\\mid\\!>"
        VarSym "<+>" -> special "<{\\mkern-12mu}+{\\mkern-12mu}>"
        VarSym "<*>" -> special "<{\\mkern-12mu}*{\\mkern-12mu}>"
        VarSym "<$>" -> special "<{\\mkern-6mu}\\${\\mkern-6mu}>"
        VarSym "++" -> special "+\\!+"
        VarSym "*^" -> thin (func "cdot")
        VarSym "-<" -> thin (func "lefttail") 
        VarSym "<-" -> thin (func "leftarrow")
        VarSym "===" -> thick (tex "≡")
        VarSym ">>=" -> thick (mathnormal (tex ">\\mkern-3mu >\\mkern-2mu ="))
        VarSym ">>" -> thick (mathnormal (tex ">\\mkern-3mu >"))
        VarSym ">>>" -> thick (mathnormal (tex ">\\mkern-3mu >\\mkern-3mu >"))
        VarSym "***" -> thick (mathnormal (tex "*\\mkern-4mu *\\mkern-4mu *"))
        VarSym "." -> (1,tex ".", 5)
        VarSym "∘" -> thick (mathnormal self)
        VarSym "×" -> thick (mathnormal self)
        VarSym "<||>" -> thin (mathnormal  (tex "<\\mkern-5mu | | \\mkern -5mu >"))
        VarSym _ -> symbol
        ConSym _ -> ident
        QVarSym _ -> ident
        QConSym _ -> ident
        IntTok _ -> literal
        FloatTok _ -> literal
        Character _ -> string
        StringTok _ -> string
        IntTokHash _ -> literal
        WordTokHash _ -> literal
        FloatTokHash _ -> literal
        DoubleTokHash _ -> literal
        CharacterHash _ -> literal
        StringHash _ -> literal
        LeftParen    -> leftParen
        RightParen -> rightParen
        LeftHashParen  -> symbol
        RightHashParen -> symbol
        SemiColon    -> symbol
        LeftCurly    -> leftParen
        RightCurly   -> rightParen
        VRightCurly -> rightParen
        LeftSquare   -> leftParen
        RightSquare          -> rightParen
        ParArrayLeftSquare   -> leftParen
        ParArrayRightSquare -> rightParen
        Comma -> rightParenMed
        Underscore -> symbol
        BackQuote -> (0,tex "`",0)
        Dot -> symbol
        DotDot -> symbol
        Colon -> symbol
        QuoteColon -> symbol
        DoubleColon -> symbol
        Equals -> symbol
        Backslash -> symbol
        Bar -> symbol
        LeftArrow -> thick $ mathnormal (func "leftarrow")
        RightArrow -> verythick $ mathnormal (func "rightarrow")
        At -> symbol
        Tilde -> symbol
        DoubleArrow -> verythick $ mathnormal (func "Rightarrow")
        Minus -> symbol
        Exclamation -> symbol
        Star -> symbol
        LeftArrowTail -> symbol
        RightArrowTail -> symbol
        LeftDblArrowTail -> symbol
        RightDblArrowTail -> symbol
        THExpQuote -> symbol
        THPatQuote -> symbol
        THDecQuote -> symbol
        THTypQuote -> symbol
        THCloseQuote -> symbol
        THIdEscape _ -> unquote
        THParenEscape -> symbol
        THVarQuote -> symbol
        THTyQuote -> symbol
        THQuasiQuote _ -> quote
        RPGuardOpen -> symbol
        RPGuardClose -> symbol
        RPCAt -> symbol
        XCodeTagOpen -> symbol
        XCodeTagClose -> symbol
        XStdTagOpen -> symbol
        XStdTagClose -> symbol
        XCloseTagOpen -> symbol
        XEmptyTagClose -> symbol
        XChildTagOpen -> symbol
        XPCDATA _ -> symbol
        XRPatOpen -> symbol
        XRPatClose -> symbol
        PragmaEnd -> symbol
        RULES -> pragma
        INLINE _ -> pragma
        INLINE_CONLIKE -> pragma
        SPECIALISE -> pragma
        SPECIALISE_INLINE _ -> pragma
        SOURCE -> pragma
        DEPRECATED -> pragma
        WARNING -> pragma
        SCC -> pragma
        GENERATED -> pragma
        CORE -> pragma
        UNPACK -> pragma
        OPTIONS _ -> pragma
        LANGUAGE -> pragma
        ANN -> pragma
        MINIMAL -> pragma
        NO_OVERLAP -> pragma
        OVERLAP -> pragma
        INCOHERENT -> pragma
        KW_As -> keyword
        KW_By -> keyword
        KW_Case -> keyword
        KW_Class -> keyword
        KW_Data -> keyword
        KW_Default -> keyword
        KW_Deriving -> keyword
        KW_Do -> keyword
        KW_MDo -> keyword
        KW_Else -> keyword
        KW_Family -> keyword
        KW_Forall -> keyword
        KW_Group -> keyword
        KW_Hiding -> keyword
        KW_If -> keyword
        KW_Import -> keyword
        KW_In -> keyword
        KW_Infix -> keyword
        KW_InfixL -> keyword
        KW_InfixR -> keyword
        KW_Instance -> keyword
        KW_Let -> keyword
        KW_Module -> keyword
        KW_NewType -> keyword
        KW_Of -> keyword
        KW_Proc -> keyword
        KW_Rec -> keyword
        KW_Then -> keyword
        KW_Type -> keyword
        KW_Using -> keyword
        KW_Where -> keyword
        KW_Qualified -> keyword
        KW_Foreign -> keyword
        KW_Export -> keyword
        KW_Safe -> keyword
        KW_Unsafe -> keyword
        KW_Threadsafe -> keyword
        KW_Interruptible -> keyword
        KW_StdCall -> keyword
        KW_CCall -> keyword
        KW_CPlusPlus -> keyword
        KW_DotNet -> keyword
        KW_Jvm -> keyword
        KW_Js -> keyword
        KW_CApi -> keyword
        _ -> debug
