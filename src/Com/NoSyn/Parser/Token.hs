module Com.NoSyn.Parser.Token where

data Token =
    TokenString String
    | TokenInt Int
    | TokenDouble Float
    | TokenChar Char
    | TokenOperator String
    | TokenComma
    | TokenParameterOpen
    | TokenParameterClose
    | TokenEquals
    | TokenAliasKeyword
    | TokenCurlyOpen
    | TokenCurlyClose
    | TokenSemicolon
    | TokenEmpty
    | TokenIdent
