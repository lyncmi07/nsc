module Com.NoSyn.Parser.Lexer where

import Com.NoSyn.Parser.Token
import Text.Ascii
import Com.NoSyn.Data.Operators
import Com.NoSyn.Error.CompilerStatus

digits = ['0','1','2','3','4','5','6','7','8','9']

monadicLexer :: (Token -> Cs a) -> Cs a
monadicLexer continueParse programSource = \originLine (lastTokenStart, lastTokenEnd) ->
    let (token, restOfProgram, newLine, (currentTokenStart, currentTokenEnd)) = lexer programSource originLine (lastTokenStart, lastTokenEnd) in
    continueParse token restOfProgram newLine (currentTokenStart, currentTokenEnd)

lexer :: String -> LineNumber -> (Column, Column) -> (Token, String, LineNumber, (Column, Column))
lexer [] = \line (_, _) -> (TokenEOF, "", line + 1, (0, 0))
lexer ('/':xs) = \line (sCol, eCol) -> lexMaybeComment xs line (eCol + 1, eCol + 1)
lexer ('\n':xs) = \line (sCol, eCol) -> lexer xs (line + 1) (0, 0)
lexer (x:xs)
    | isWhiteSpace x = \line (sCol, eCol) -> lexer xs line (sCol + 1, eCol + 1)
    | isAlpha x = lexVar (x:xs)
    | x `elem` digits = lexNum (x:xs)
    | x `elem` operatorChars = lexOperator (x:xs)
lexer ('"':xs) = lexString xs
lexer ('`':xs) = lexNativeCode xs

lexer (',':xs) = lexReturningFunction "," TokenComma xs
lexer (')':xs) = lexReturningFunction ")" TokenParameterClose xs
lexer ('=':xs) = lexReturningFunction "=" TokenEquals xs
lexer ('}':xs) = lexReturningFunction "}" TokenCurlyClose xs
lexer (']':xs) = lexReturningFunction "]" TokenSquareClose xs
lexer (';':xs) = lexReturningFunction ";" TokenSemicolon xs
lexer ('_':xs) = lexReturningFunction "_" TokenUnderscore xs
lexer ('{':xs) = lexReturningFunction "{" TokenCurlyOpen xs
lexer ('(':xs) = lexReturningFunction "(" TokenParameterOpen xs
lexer ('[':xs) = lexReturningFunction "[" TokenSquareOpen xs

lexReturningFunction tokenString token restOfProgram = 
    \line (sCol, eCol) -> (token, restOfProgram, line, (eCol+1, eCol+1+(length tokenString)))

lexVar :: String -> LineNumber -> (Column, Column) -> (Token, String, LineNumber, (Column, Column))
lexVar xs = case span isAlpha xs of
    ("alias", rest)         -> lexReturningFunction "alias" TokenAliasKeyword rest
    ("prefix", rest)        -> lexReturningFunction "prefix" TokenPrefixKeyword rest
    ("postfix", rest)       -> lexReturningFunction "postfix" TokenPostfixKeyword rest
    ("infix", rest)         -> lexReturningFunction "infix" TokenInfixKeyword rest
    ("bracketop", rest)     -> lexReturningFunction "bracketop" TokenBracketOpKeyword rest
    ("native", rest)        -> lexReturningFunction "native" TokenNativeKeyword rest
    ("import", rest)        -> lexReturningFunction "import" TokenImportKeyword rest
    (identPrefix, restOfIdent) -> \line (sCol, eCol) -> 
        let (ident, rest) = span (\x -> isAlpha x || x == '_') (identPrefix ++ restOfIdent) in
            (TokenIdent ident, rest, line, (sCol, eCol))
            
lexString x = \line (sCol, eCol) -> lexString' x "" line (eCol + 1, eCol + 1)
lexString' ('"':xs) finalString = \line (sCol, eCol) -> ((TokenString finalString), xs, line, (sCol, eCol+1))
lexString' ('\n':xs) currentString = \line (sCol, eCol) -> lexString' xs (currentString ++ ['\n']) (line + 1) (0, 0)
lexString' (x:xs) currentString = \line (sCol, eCol) -> lexString' xs (currentString ++ [x]) line (sCol, eCol + 1)

lexNativeCode x = \line (sCol, eCol) -> lexNativeCode' x "" line (sCol, eCol)
lexNativeCode' ('`':xs) finalNativeCode = \line (sCol, eCol) -> (TokenNativeCode finalNativeCode, xs, line, (sCol, eCol + 1))
lexNativeCode' ('\n':xs) currentNativeCode = \line (sCol, eCol) -> lexNativeCode' xs (currentNativeCode ++ ['\n']) (line + 1) (0, 0)
lexNativeCode' (x:xs) currentNativeCode = \line (sCol, eCol) -> lexNativeCode' xs (currentNativeCode ++ [x]) line (sCol, eCol + 1)


lexNum x = \line (sCol, eCol) -> lexNum' x "" line (eCol + 1, eCol + 1)
lexNum' ('.':xs) currentInt = \line (sCol, eCol) -> lexDouble xs (currentInt ++ ".") line (sCol, eCol + 1)
lexNum' (x:xs) currentInt
    | x `elem` digits = \line (sCol, eCol) -> lexNum' xs (currentInt++[x]) line (sCol, eCol + 1)
    | otherwise = \line (sCol, eCol) -> (TokenInt (read currentInt :: Int), x:xs, line, (sCol, eCol))

lexDouble (x:xs) currentInt
    | x `elem` digits = \line (sCol, eCol) -> lexDouble xs (currentInt++[x]) line (sCol, eCol + 1)
    | otherwise = \line (sCol, eCol) -> (TokenDouble (read currentInt :: Double), x:xs, line, (sCol, eCol))

lexOperator x = lexOperator' x ""
lexOperator' (x:xs) currentOperator
    | x `elem` operatorChars = \line (sCol, eCol) -> lexOperator' xs (currentOperator++[x]) line (sCol, eCol + 1)
    | otherwise = \line (sCol, eCol) -> (TokenOperator currentOperator, x:xs, line, (sCol, eCol))

lexMaybeComment ('/':xs) = \line (sCol, eCol) -> lexComment xs line (sCol, eCol + 1)
lexMaybeComment xs =  lexOperator ('/':xs)

lexComment :: String -> LineNumber -> (Column, Column) -> (Token, String, LineNumber, (Column, Column))
lexComment ('\n':xs) = \line (_, _) -> lexer xs (line + 1) (0, 0)
lexComment (_:xs) = \line (sCol, eCol) -> lexComment xs line (sCol, eCol + 1)
lexComment [] = lexer []


closingBracket '{' = '}'
closingBracket '(' = ')'
closingBracket '[' = ']'


