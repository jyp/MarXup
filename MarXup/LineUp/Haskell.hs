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


haskell :: Verbatim a -> Tex ()
haskell = haskellCust defaultParseMode printTok

haskellCust :: ParseMode -> (Token -> TeX) -> Verbatim a -> Tex ()
haskellCust mode custPrintTok v = case lexTokenStreamWithMode mode (fromVerbatim v) of
  ParseOk toks -> lineup (map (map tokInfo) lins)
    where lins = groupBy ((==) `on` (srcSpanStartLine . loc)) toks
          tokInfo :: Loc Token -> Tok
          tokInfo (Loc l t) = Tok (srcSpanStartColumn l) (srcSpanEndColumn l) (custPrintTok t)
  ParseFailed location err -> textual (show location ++ show err)

printTok :: Token -> TeX
printTok t = let s = textual $ showToken t
                 ident = cmd "mathsf" s
                 unquote = cmd "mathsf" s
                 quote = cmd "mathtt" s
                 literal = cmd "mathrm" s
                 keyword = cmd "mathbf" s
                 pragma = cmd "mathrm" s
                 symbol = cmd "mathnormal" s
  in case t of
        -- _ -> cmd "mathrm" $ textual $ show t -- Debug
        VarId _ -> ident
        QVarId _ -> ident
        IDupVarId _ -> ident
        ILinVarId _ -> ident
        ConId _ -> ident
        QConId _ -> ident
        DVarId _ -> ident
        VarSym "<|>" -> tex "<\\!\\|\\!>"
        ConSym _ -> ident
        QVarSym _ -> ident
        QConSym _ -> ident
        IntTok _ -> literal
        FloatTok _ -> literal
        Character _ -> literal
        StringTok _ -> literal
        IntTokHash _ -> literal
        WordTokHash _ -> literal
        FloatTokHash _ -> literal
        DoubleTokHash _ -> literal
        CharacterHash _ -> literal
        StringHash _ -> literal
        LeftParen    -> symbol
        RightParen -> symbol
        LeftHashParen        -> symbol
        RightHashParen -> symbol
        SemiColon    -> symbol
        LeftCurly    -> symbol
        RightCurly   -> symbol
        VRightCurly -> symbol
        LeftSquare   -> symbol
        RightSquare          -> symbol
        ParArrayLeftSquare   -> symbol
        ParArrayRightSquare -> symbol
        Comma -> symbol
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
        LeftArrow -> cmd0 "leftarrow"
        RightArrow -> cmd0 "rightarrow"
        At -> symbol
        Tilde -> symbol
        DoubleArrow -> cmd0 "Leftarrow"
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
        _ -> cmd "mathnormal" s
