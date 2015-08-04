module MarXup.LineUp.Haskell where

import Data.List
import Data.Function
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser (ParseResult(..),ParseMode(..),defaultParseMode)
import Language.Haskell.Exts.SrcLoc
import MarXup
import MarXup.LineUp
import MarXup.Tex
import MarXup.Verbatim
import Data.Monoid

haskell :: Verbatim a -> Tex ()
haskell = haskellCust defaultParseMode printTok

haskellInline :: Verbatim a -> Tex ()
haskellInline = haskellInlineCust defaultParseMode printTok

type PrintTok = Token -> (Float,TeX,Float)

haskellInlineCust :: ParseMode -> (PrintTok) -> Verbatim a -> Tex ()
haskellInlineCust mode custPrintTok v = case lexTokenStreamWithMode mode (fromVerbatim v) of
   ParseOk toks -> mconcat $ map render $ mkSpaces $ map (mkTok custPrintTok) toks
   ParseFailed location err -> textual (show location ++ show err)

mkTok :: (t -> (Float, TeX, Float)) -> Loc t -> Tok
mkTok custPrintTok (Loc l t) = Tok (srcSpanStartColumn l) (srcSpanEndColumn l) before txt after
  where (before,txt,after) = custPrintTok t

haskellCust :: ParseMode -> (PrintTok) -> Verbatim a -> Tex ()
haskellCust mode custPrintTok v = case lexTokenStreamWithMode mode (fromVerbatim v) of
  ParseOk toks -> lineup (map (map (mkTok custPrintTok)) lins)
    where lins = groupBy ((==) `on` (srcSpanStartLine . loc)) toks
  ParseFailed location err -> textual (show location ++ show err)

printTok :: PrintTok
printTok t = let s = textual $ showToken t
                 ident = regular $ cmd "mathsf" s
                 unquote = regular $ cmd "mathsf" s
                 quote = regular $ cmd "mathtt" s
                 literal = regular $ cmd "mathrm" s
                 string = regular $ cmd "texttt" s
                 keyword = regular $ cmd "mathbf" s
                 pragma = regular $ cmd "mathrm" s
                 symbol = regular $ cmd "mathnormal" s
                 regular tx = (5,tx,5)
                 leftParen  = (5,cmd "mathnormal" s,0)
                 rightParen = (0,cmd "mathnormal" s,5)
                 special x = regular $ cmd "mathnormal" $ tex x
                 debug = regular $ textual "[" <> ( cmd "mathnormal" $ textual $ show t) <> textual "]"
  in case t of
        -- _ -> cmd "mathrm" $ textual $ show t -- Debug
        VarId _ -> ident
        QVarId _ -> ident
        IDupVarId _ -> ident
        ILinVarId _ -> ident
        ConId _ -> ident
        QConId _ -> ident
        DVarId _ -> ident
        VarSym "<>" -> special "<\\!>"
        VarSym "<|>" -> special "<\\!\\mid\\!>"
        VarSym "<$>" -> special "<\\!\\mid\\!>"
        VarSym "<+>" -> special "<{\\mkern-12mu}+{\\mkern-12mu}>"
        VarSym "++" -> special "+\\!+"
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
        Comma -> rightParen
        Underscore -> symbol
        BackQuote -> symbol
        Dot -> symbol
        DotDot -> symbol
        Colon -> symbol
        QuoteColon -> symbol
        DoubleColon -> symbol
        Equals -> symbol
        Backslash -> symbol
        Bar -> symbol
        LeftArrow -> regular $ cmd0 "leftarrow"
        RightArrow -> regular $ cmd0 "rightarrow"
        At -> symbol
        Tilde -> symbol
        DoubleArrow -> regular $ cmd0 "Rightarrow"
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
