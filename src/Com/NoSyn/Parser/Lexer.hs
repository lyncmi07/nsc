module Com.NoSyn.Parser.Lexer where

import Com.NoSyn.Parser.Token
import Text.Ascii
import Com.NoSyn.Data.Operators
import Com.NoSyn.Error.CompilerStatus

digits = ['0','1','2','3','4','5','6','7','8','9']

monadicLexer :: (Token -> Cs a) -> Cs a
monadicLexer continueParse programSource =
    let (token, restOfProgram) = lexer programSource in
    continueParse token restOfProgram

lexer :: String -> (Token, String)
lexer [] = (TokenEOF, "")
lexer ('/':xs) = lexMaybeComment xs
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

lexer (',':xs) = (TokenComma, xs)
lexer (')':xs) = (TokenParameterClose, xs)
lexer ('=':xs) = (TokenEquals, xs)
lexer ('}':xs) = (TokenCurlyClose, xs)
lexer (']':xs) = (TokenSquareClose, xs)
lexer (';':xs) = (TokenSemicolon, xs)
lexer ('_':xs) = (TokenUnderscore, xs)
lexer ('{':xs) = (TokenCurlyOpen, xs)
lexer ('(':xs) = (TokenParameterOpen, xs)
lexer ('[':xs) = (TokenSquareOpen, xs)

lexVar xs = case span isAlpha xs of
    ("alias", rest)         -> (TokenAliasKeyword, rest)
    ("prefix", rest)        -> (TokenPrefixKeyword, rest)
    ("postfix", rest)       -> (TokenPostfixKeyword, rest)
    ("infix", rest)         -> (TokenInfixKeyword, rest)
    ("bracketop", rest)     -> (TokenBracketOpKeyword, rest)
    ("native", rest)        -> (TokenNativeKeyword, rest)
    ("import", rest)        -> (TokenImportKeyword, rest)
    (identPrefix, restOfIdent) ->
        let (ident, rest) = span (\x -> isAlpha x || x == '_') (identPrefix ++ restOfIdent) in
            (TokenIdent ident, rest)
            
lexString x = let (stringToken, rest) = lexString' x "" in (stringToken, rest)
lexString' :: String -> String -> (Token, String)
lexString' ('"':xs) finalString = ((TokenString finalString), xs)
lexString' (x:xs) currentString = lexString' xs (currentString ++ [x])

lexNativeCode x = let (nativeCodeToken, rest) = lexNativeCode' x "" in (nativeCodeToken, rest)
lexNativeCode' :: String -> String -> (Token, String)
lexNativeCode' ('`':xs) finalNativeCode = (TokenNativeCode finalNativeCode, xs)
lexNativeCode' (x:xs) currentNativeCode = lexNativeCode' xs (currentNativeCode ++ [x])


lexNum x = let (numToken, rest) = lexNum' x "" in (numToken, rest)
lexNum' :: String -> String -> (Token, String)
lexNum' ('.':xs) currentInt = lexDouble xs (currentInt ++ ".")
lexNum' (x:xs) currentInt
    | x `elem` digits = lexNum' xs (currentInt++[x])
    | otherwise = (TokenInt (read currentInt :: Int), x:xs)

lexDouble :: String -> String -> (Token, String)
lexDouble (x:xs) currentInt
    | x `elem` digits = lexDouble xs (currentInt++[x])
    | otherwise = (TokenDouble (read currentInt :: Double), x:xs)

lexOperator x = let (operatorToken, rest) = lexOperator' x "" in (operatorToken, rest)
lexOperator' :: String -> String -> (Token, String)
lexOperator' (x:xs) currentOperator
    | x `elem` operatorChars = lexOperator' xs (currentOperator++[x])
    | otherwise = (TokenOperator currentOperator, x:xs)

-- lexBracket bracket (x:xs)
    -- | isWhiteSpace x = lexBracket bracket xs
    -- | x == (closingBracket bracket) = ([bracketOpenToken bracket, TokenEmpty, bracketCloseToken bracket], xs)
    -- | otherwise = ([bracketOpenToken bracket], x:xs)

lexMaybeComment ('/':xs) = lexComment xs
lexMaybeComment xs = lexOperator ('/':xs)

lexComment ('\n':xs) = lexer xs
lexComment (_:xs) = lexComment xs
lexComment [] = lexer []


closingBracket '{' = '}'
closingBracket '(' = ')'
closingBracket '[' = ']'


