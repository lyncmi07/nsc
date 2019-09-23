module Com.NoSyn.Parser.Lexer where

import Com.NoSyn.Parser.Token
import Text.Ascii
import Com.NoSyn.Data.Operators
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Parser.TokenLength

digits = ['0','1','2','3','4','5','6','7','8','9']

monadicLexer :: (Token -> Cs a) -> Cs a
monadicLexer continueParse programSource = \originCol originLine originTokenPositions ->
    let (token, restOfProgram, newCol, newLine, newTokenPositions) = lexer programSource originCol originLine originTokenPositions in
        continueParse token restOfProgram newCol newLine newTokenPositions

lexer :: String -> Column -> LineNumber -> [(LineNumber, Column, LineNumber, Column)] -> (Token, String, Column, LineNumber, [(LineNumber, Column, LineNumber, Column)])
lexer [] = \currentCol line tokenPositions -> (TokenEOF, "", currentCol, line, tokenPositions)
lexer ('/':xs) = \currentCol -> lexMaybeComment xs (currentCol + 1)
lexer ('\n':xs) = \_ line -> lexer xs 0 (line + 1)
lexer (x:xs)
    | isWhiteSpace x = \currentCol -> lexer xs (currentCol + 1)
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
    \currentCol line tokenPositions ->
        (token, restOfProgram, currentCol+(length tokenString), line, tokenPositions ++ [(line, currentCol, line, currentCol + (length tokenString) - 1)])

lexVar xs = case span isAlpha xs of
    ("alias", rest)         -> lexReturningFunction "alias" TokenAliasKeyword rest
    ("prefix", rest)        -> lexReturningFunction "prefix" TokenPrefixKeyword rest
    ("postfix", rest)       -> lexReturningFunction "postfix" TokenPostfixKeyword rest
    ("infix", rest)         -> lexReturningFunction "infix" TokenInfixKeyword rest
    ("bracketop", rest)     -> lexReturningFunction "bracketop" TokenBracketOpKeyword rest
    ("native", rest)        -> lexReturningFunction "native" TokenNativeKeyword rest
    ("import", rest)        -> lexReturningFunction "import" TokenImportKeyword rest
    (identPrefix, restOfIdent) -> \currentCol line tokenPositions ->
        let (ident, rest) = span (\x -> isAlpha x || x == '_') (identPrefix ++ restOfIdent) in
            (TokenIdent ident, rest, currentCol + (length ident), line, tokenPositions ++ [(line, currentCol, line, currentCol + (length ident) - 1)])
            
lexString x = \startCol startLine tokenPositions -> lexString' x "" (startCol + 1) startLine tokenPositions startCol startLine
lexString' ('"':xs) finalString = \currentCol currentLine tokenPositions startCol startLine ->
    (TokenString finalString, xs, (currentCol+2), currentLine, tokenPositions ++ [(startLine, startCol, currentLine, currentCol + 1)])
lexString' ('\n':xs) currentString = \_ currentLine -> lexString' xs (currentString ++ "\n") 0 (currentLine + 1)
lexString' (x:xs) currentString = \currentCol -> lexString' xs (currentString ++ [x]) (currentCol + 1)

lexNativeCode x = \startCol startLine tokenPositions -> lexNativeCode' x "" (startCol + 1) startLine tokenPositions startCol startLine
lexNativeCode' ('`':xs) finalNativeCode = \currentCol currentLine tokenPositions startCol startLine->
    (TokenNativeCode finalNativeCode, xs, (currentCol + 2), currentLine, tokenPositions ++ [(startLine, startCol, currentLine, currentCol + 1)])
lexNativeCode' ('\n':xs) currentNativeCode = \_ line -> lexNativeCode' xs (currentNativeCode ++ "\n") 0 (line + 1)
lexNativeCode' (x:xs) currentNativeCode = \currentCol -> lexNativeCode' xs (currentNativeCode ++ [x]) (currentCol + 1)

lexNum x = \startCol line tokenPositions -> lexNum' x "" startCol line tokenPositions startCol
lexNum' ('.':xs) currentInt = \currentCol line tokenPositions startCol-> lexDouble xs (currentInt ++ ".") (currentCol + 1) line tokenPositions startCol
lexNum' (x:xs) currentInt
    | x `elem` digits = \currentCol -> lexNum' xs (currentInt++[x]) (currentCol + 1)
    | otherwise = \currentCol line tokenPositions startCol ->
        (TokenInt (read currentInt :: Int), x:xs, currentCol, line, tokenPositions ++ [(line, startCol, line, currentCol - 1)])

lexDouble (x:xs) currentInt
    | x `elem` digits = \currentCol -> lexDouble xs (currentInt ++ [x]) (currentCol + 1)
    | otherwise  = \currentCol line tokenPositions startCol ->
        (TokenDouble (read currentInt :: Double), x:xs, currentCol, line, tokenPositions ++ [(line, startCol, line, currentCol - 1)])

lexOperator x = \startCol line tokenPositions -> lexOperator' x "" startCol line tokenPositions startCol
lexOperator' (x:xs) currentOperator
    | x `elem` operatorChars = \currentCol -> lexOperator' xs (currentOperator ++ [x]) (currentCol + 1)
    | otherwise = \currentCol line tokenPositions startCol ->
        (TokenOperator currentOperator, x:xs, currentCol, line, tokenPositions ++ [(line, startCol+1, line, currentCol)])

lexMaybeComment ('/':xs) = lexComment xs

lexComment ('\n':xs) = \_ line -> lexer xs 0 (line + 1)
lexComment (_:xs) = lexComment xs
lexComment [] = lexer []


closingBracket '{' = '}'
closingBracket '(' = ')'
closingBracket '[' = ']'


