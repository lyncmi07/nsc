module Com.NoSyn.Parser.Token where

data Token =
    TokenString String
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
    | TokenBracketOpKeyword
    | TokenSemicolon
    | TokenEmpty
    | TokenUnderscore
    deriving Show
