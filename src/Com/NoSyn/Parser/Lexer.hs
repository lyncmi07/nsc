module Com.NoSyn.Parser.Lexer where

import Com.NoSyn.Parser.Token
import Text.Ascii
import Com.NoSyn.Data.Operators

digits = ['0','1','2','3','4','5','6','7','8','9']

lexer :: String -> [Token]
lexer [] = [TokenEmpty]
lexer ('/':xs) = lexMaybeComment xs
lexer (x:xs)
    | isWhiteSpace x = lexer xs
    | isAlpha x = lexVar (x:xs)
    | x `elem` digits = lexNum (x:xs)
    | x `elem` operatorChars = lexOperator (x:xs)
lexer ('"':xs) = lexString xs
lexer ('`':xs) = lexNativeCode xs
lexer (x:xs)
    | x `elem` ['(', '[', '{'] = let (tokens, rest) = lexBracket x xs in
        tokens ++ lexer rest

lexer (',':xs) = TokenComma : lexer xs
lexer (')':xs) = TokenParameterClose : lexer xs
lexer ('=':xs) = TokenEquals : lexer xs
lexer ('}':xs) = TokenCurlyClose : lexer xs
lexer (']':xs) = TokenSquareClose : lexer xs
lexer (';':xs) = TokenSemicolon : lexer xs
lexer ('_':xs) = TokenUnderscore : lexer xs

lexVar xs = case span isAlpha xs of
    ("alias", rest)         -> TokenAliasKeyword : lexer rest
    ("prefix", rest)        -> TokenPrefixKeyword : lexer rest
    ("postfix", rest)       -> TokenPostfixKeyword : lexer rest
    ("infix", rest)         -> TokenInfixKeyword : lexer rest
    ("bracketop", rest)     -> TokenBracketOpKeyword : lexer rest
    ("native", rest)        -> TokenNativeKeyword : lexer rest
    ("import", rest)        -> TokenImportKeyword : lexer rest
    (identPrefix, restOfIdent) ->
        let (ident, rest) = span (\x -> isAlpha x || x == '_') (identPrefix ++ restOfIdent) in
            TokenIdent ident : lexer rest
            
lexString x = let (stringToken, rest) = lexString' x "" in stringToken : lexer rest
lexString' :: String -> String -> (Token, String)
lexString' ('"':xs) finalString = (TokenString finalString, xs)
lexString' (x:xs) currentString = lexString' xs (currentString ++ [x])

lexNativeCode x = let (nativeCodeToken, rest) = lexNativeCode' x "" in nativeCodeToken : lexer rest
lexNativeCode' :: String -> String -> (Token, String)
lexNativeCode' ('`':xs) finalNativeCode = (TokenNativeCode finalNativeCode, xs)
lexNativeCode' (x:xs) currentNativeCode = lexNativeCode' xs (currentNativeCode ++ [x])


lexNum x = let (numToken, rest) = lexNum' x "" in numToken : lexer rest
lexNum' :: String -> String -> (Token, String)
lexNum' ('.':xs) currentInt = lexDouble xs (currentInt ++ ".")
lexNum' (x:xs) currentInt
    | x `elem` digits = lexNum' xs (currentInt++[x])
    | otherwise = (TokenInt (read currentInt :: Int), x:xs)

lexDouble :: String -> String -> (Token, String)
lexDouble (x:xs) currentInt
    | x `elem` digits = lexDouble xs (currentInt++[x])
    | otherwise = (TokenDouble (read currentInt :: Double), x:xs)

lexOperator x = let (operatorToken, rest) = lexOperator' x "" in operatorToken : lexer rest
lexOperator' :: String -> String -> (Token, String)
lexOperator' (x:xs) currentOperator
    | x `elem` operatorChars = lexOperator' xs (currentOperator++[x])
    | otherwise = (TokenOperator currentOperator, x:xs)

lexBracket bracket (x:xs)
    | isWhiteSpace x = lexBracket bracket xs
    | x == (closingBracket bracket) = ([bracketOpenToken bracket, TokenEmpty, bracketCloseToken bracket], xs)
    | otherwise = ([bracketOpenToken bracket], x:xs)

lexMaybeComment ('/':xs) = lexComment xs
lexMaybeComment xs = lexOperator ('/':xs)

lexComment ('\n':xs) = lexer xs
lexComment (_:xs) = lexComment xs
lexComment [] = lexer []


closingBracket '{' = '}'
closingBracket '(' = ')'
closingBracket '[' = ']'

bracketOpenToken '{' = TokenCurlyOpen
bracketOpenToken '(' = TokenParameterOpen
bracketOpenToken '[' = TokenSquareOpen
bracketCloseToken '{' = TokenCurlyClose
bracketCloseToken '(' = TokenParameterClose
bracketCloseToken '[' = TokenSquareClose

