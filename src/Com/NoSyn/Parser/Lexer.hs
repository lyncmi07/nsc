module Com.NoSyn.Parser.Lexer where

import Com.NoSyn.Parser.Token

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | isSpace c = lexer xs
    | isAlpha c lexVar (x:xs)
    | isDigit c = lexNum (x:xs)
lexer ('"':xs) = lexString xs
lexer ('(':')':xs) = [TokenParameterOpen, TokenEmpty, TokenParameterClose] ++ lexer xs
lexer ('[':']':xs) = [TokenSquareOpen, TokenEmpty, TokenSquareOpen] ++ lexer xs
lexer ('{':'}':xs) = [TokenCurlyOpen, TokenEmpty, TokenCurlyClose] ++ lexer xs
lexer (',':xs) = TokenComma : lexer xs
lexer ('(':xs) = TokenParameterOpen : lexer xs
lexer (')':xs) = TokenParameterClose : lexer xs
lexer ('=':xs) = TokenEquals : lexer xs
lexer ('{':xs) = TokenCurlyOpen : lexer xs
lexer ('}':xs) = TokenCurlyClose : lexer xs
lexer ('[':xs) = TokenSquareOpen : lexer xs
lexer (']':xs) = TokenSquareClose : lexer xs
lexer (';':xs) = TokenSemicolon : lexer xs
lexer ('_':xs) = TokenUnderscore : lexer xs

lexVar xs = case span isAlpha xs of
    ("alias", rest)         -> TokenAliasKeyword : lexer rest
    ("prefix", rest)        -> TokenPrefixKeyword : lexer rest
    ("postfix", rest)       -> TokenPostfixKeyword : lexer rest
    ("infix", rest)         -> TokenInfixKeyword : lexer rest
    ("bracketop", rest)     -> TokenBracketOpKeyword : lexer rest
    (ident, rest)           -> TokenIdent ident

lexString x = let (stringToken, rest) = lexString' x "" in stringToken : lexer rest
lexString' :: String -> String -> (Token, String)
lexString' ('"':xs) finalString = (TokenString finalString, xs)
lexString' (x:xs) currentString = lexString' xs (x:currentString)

digits = ['0','1','2','3','4','5','6','7','8','9']
lexNum x = let (numToken, rest) = lexNum' x "" in numToken : lexer rest
lexNum' :: String -> String -> (Token, String)
lexNum' ('.':xs) currentInt = lexDouble xs (currentInt )
lexNum' (x:xs) currentInt
    | x `elem` digits = lexNum' xs (x:currentInt)
    | otherwise = TokenInt (read currentInt :: Int) : lexer (x:xs)

lexDouble a = let (numToken, rest) = lexDouble' x "" in numToken : lexer rest
lexDouble :: String -> String -> (Token, String)
lexNum' (x:xs) currentInt
    | x `elem` digits = lexNum' xs (x:currentInt)
    | otherwise = TokenInt (read currentInt :: Int) : lexer (x:xs)



