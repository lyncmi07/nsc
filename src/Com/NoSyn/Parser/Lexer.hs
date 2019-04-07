module Com.NoSyn.Parser.Lexer where

import Com.NoSyn.Parser.Token
import Text.Ascii
import Com.NoSyn.Data.Operators

digits = ['0','1','2','3','4','5','6','7','8','9']

lexer :: String -> [Token]
lexer [] = [TokenEmpty]
lexer (x:xs)
    | isWhiteSpace x = lexer xs
    | isAlpha x = lexVar (x:xs)
    | x `elem` digits = lexNum (x:xs)
    | x `elem` operatorChars = lexOperator (x:xs)
lexer ('"':xs) = lexString xs
-- lexer ('(':')':xs) = [TokenParameterOpen, TokenEmpty, TokenParameterClose] ++ lexer xs
-- lexer ('[':']':xs) = [TokenSquareOpen, TokenEmpty, TokenSquareOpen] ++ lexer xs
-- lexer ('{':'}':xs) = [TokenCurlyOpen, TokenEmpty, TokenCurlyClose] ++ lexer xs
lexer (x:xs)
    | x `elem` ['(', '[', '{'] = let (tokens, rest) = lexBracket x xs in
        tokens ++ lexer rest
lexer (',':xs) = TokenComma : lexer xs
-- lexer ('(':xs) = TokenParameterOpen : lexer xs
lexer (')':xs) = TokenParameterClose : lexer xs
lexer ('=':xs) = TokenEquals : lexer xs
-- lexer ('{':xs) = TokenCurlyOpen : lexer xs
lexer ('}':xs) = TokenCurlyClose : lexer xs
-- lexer ('[':xs) = TokenSquareOpen : lexer xs
lexer (']':xs) = TokenSquareClose : lexer xs
lexer (';':xs) = TokenSemicolon : lexer xs
lexer ('_':xs) = TokenUnderscore : lexer xs

lexVar xs = case span isAlpha xs of
    ("alias", rest)         -> TokenAliasKeyword : lexer rest
    ("prefix", rest)        -> TokenPrefixKeyword : lexer rest
    ("postfix", rest)       -> TokenPostfixKeyword : lexer rest
    ("infix", rest)         -> TokenInfixKeyword : lexer rest
    ("bracketop", rest)     -> TokenBracketOpKeyword : lexer rest
    (ident, rest)           -> TokenIdent ident : lexer rest

lexString x = let (stringToken, rest) = lexString' x "" in stringToken : lexer rest
lexString' :: String -> String -> (Token, String)
lexString' ('"':xs) finalString = (TokenString finalString, xs)
lexString' (x:xs) currentString = lexString' xs (x:currentString)

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


closingBracket '{' = '}'
closingBracket '(' = ')'
closingBracket '[' = ']'

bracketOpenToken '{' = TokenCurlyOpen
bracketOpenToken '(' = TokenParameterOpen
bracketOpenToken '[' = TokenSquareOpen
bracketCloseToken '{' = TokenCurlyClose
bracketCloseToken '(' = TokenParameterClose
bracketCloseToken '[' = TokenSquareClose

