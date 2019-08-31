module Com.NoSyn.Parser.Token where

data Token =
    TokenString String
    | TokenNativeCode String
    | TokenInt Int
    | TokenDouble Double
    | TokenChar Char
    | TokenOperator String
    | TokenIdent String
    | TokenComma
    | TokenParameterOpen
    | TokenParameterClose
    | TokenCurlyOpen
    | TokenCurlyClose
    | TokenSquareOpen
    | TokenSquareClose
    | TokenEquals
    | TokenAliasKeyword
    | TokenPrefixKeyword
    | TokenPostfixKeyword
    | TokenInfixKeyword
    | TokenNativeKeyword
    | TokenBracketOpKeyword
    | TokenImportKeyword
    | TokenSemicolon
    | TokenEmpty
    | TokenUnderscore
    deriving Show
