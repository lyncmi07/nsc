module Com.NoSyn.Parser.Lexer where

import Com.NoSyn.Parser.Token
import Text.Ascii
import Com.NoSyn.Data.Operators
import Com.NoSyn.Error.CompilerStatus

digits = ['0','1','2','3','4','5','6','7','8','9']

monadicLexer :: (Token -> Cs a) -> Cs a
monadicLexer continueParse programSource = \originLine ->
    let (token, restOfProgram, newLine) = lexer programSource originLine in
    continueParse token restOfProgram newLine

lexer :: String -> LineNumber -> (Token, String, LineNumber)
lexer [] = \line -> (TokenEOF, "", line + 1)
lexer ('/':xs) = \line -> lexMaybeComment xs line
lexer ('\n':xs) = \line -> lexer xs (line + 1)
lexer (x:xs)
    | isWhiteSpace x = lexer xs
    | isAlpha x = lexVar (x:xs)
    | x `elem` digits = lexNum (x:xs)
    | x `elem` operatorChars = lexOperator (x:xs)
lexer ('"':xs) = lexString xs
lexer ('`':xs) = lexNativeCode xs
-- lexer (x:xs)
    -- | x `elem` ['(', '[', '{'] = let (tokens, rest) = lexBracket x xs in
        -- (tokens, rest)

lexer (',':xs) = \line -> (TokenComma, xs, line)
lexer (')':xs) = \line -> (TokenParameterClose, xs, line)
lexer ('=':xs) = \line -> (TokenEquals, xs, line)
lexer ('}':xs) = \line -> (TokenCurlyClose, xs, line)
lexer (']':xs) = \line -> (TokenSquareClose, xs, line)
lexer (';':xs) = \line -> (TokenSemicolon, xs, line)
lexer ('_':xs) = \line -> (TokenUnderscore, xs, line)
lexer ('{':xs) = \line -> (TokenCurlyOpen, xs, line)
lexer ('(':xs) = \line -> (TokenParameterOpen, xs, line)
lexer ('[':xs) = \line -> (TokenSquareOpen, xs, line)

lexVar xs = \line -> case span isAlpha xs of
    ("alias", rest)         -> (TokenAliasKeyword, rest, line)
    ("prefix", rest)        -> (TokenPrefixKeyword, rest, line)
    ("postfix", rest)       -> (TokenPostfixKeyword, rest, line)
    ("infix", rest)         -> (TokenInfixKeyword, rest, line)
    ("bracketop", rest)     -> (TokenBracketOpKeyword, rest, line)
    ("native", rest)        -> (TokenNativeKeyword, rest, line)
    ("import", rest)        -> (TokenImportKeyword, rest, line)
    (identPrefix, restOfIdent) ->
        let (ident, rest) = span (\x -> isAlpha x || x == '_') (identPrefix ++ restOfIdent) in
            (TokenIdent ident, rest, line)
            
lexString x = lexString' x ""
lexString' ('"':xs) finalString = \line -> ((TokenString finalString), xs, line)
lexString' ('\n':xs) currentString = \line -> lexString' xs (currentString ++ ['\n']) (line + 1)
lexString' (x:xs) currentString = lexString' xs (currentString ++ [x])

lexNativeCode x = lexNativeCode' x ""
lexNativeCode' ('`':xs) finalNativeCode = \line -> (TokenNativeCode finalNativeCode, xs, line)
lexNativeCode' ('\n':xs) currentNativeCode = \line -> lexNativeCode' xs (currentNativeCode ++ ['\n']) (line + 1)
lexNativeCode' (x:xs) currentNativeCode = lexNativeCode' xs (currentNativeCode ++ [x])


lexNum x = lexNum' x ""
lexNum' ('.':xs) currentInt = lexDouble xs (currentInt ++ ".")
lexNum' (x:xs) currentInt
    | x `elem` digits = lexNum' xs (currentInt++[x])
    | otherwise = \line -> (TokenInt (read currentInt :: Int), x:xs, line)

lexDouble (x:xs) currentInt
    | x `elem` digits = lexDouble xs (currentInt++[x])
    | otherwise = \line -> (TokenDouble (read currentInt :: Double), x:xs, line)

lexOperator x = lexOperator' x ""
lexOperator' :: String -> String -> LineNumber -> (Token, String, LineNumber)
lexOperator' (x:xs) currentOperator
    | x `elem` operatorChars = lexOperator' xs (currentOperator++[x])
    | otherwise = \line -> (TokenOperator currentOperator, x:xs, line)

-- lexBracket bracket (x:xs)
    -- | isWhiteSpace x = lexBracket bracket xs
    -- | x == (closingBracket bracket) = ([bracketOpenToken bracket, TokenEmpty, bracketCloseToken bracket], xs)
    -- | otherwise = ([bracketOpenToken bracket], x:xs)

lexMaybeComment ('/':xs) = lexComment xs
lexMaybeComment xs =  lexOperator ('/':xs)

lexComment ('\n':xs) = \line -> lexer xs (line + 1)
lexComment (_:xs) = lexComment xs
lexComment [] = lexer []


closingBracket '{' = '}'
closingBracket '(' = ')'
closingBracket '[' = ']'


