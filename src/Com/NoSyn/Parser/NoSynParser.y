{
module Com.NoSyn.Parser.NoSynParser where

import Com.NoSyn.Data.Operators
import Com.NoSyn.Data.Types
import Com.NoSyn.Parser.ConcreteSyntaxTree
import Com.NoSyn.Parser.Lexer
import Com.NoSyn.Parser.Token
import Com.NoSyn.Error.CompilerContext
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.NonFatalError
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { Cs } { thenCs } { returnCs }
%lexer { monadicLexer } { TokenEOF }

%token
    string     { TokenString $$ }
    nativecode { TokenNativeCode $$ }
    integer    { TokenInt $$ }
    double     { TokenDouble $$ }
    char       { TokenChar $$ }
    operator   { TokenOperator $$ }
    ','        { TokenComma }
    '('        { TokenParameterOpen }
    ')'        { TokenParameterClose }
    '='        { TokenEquals }
    alias      { TokenAliasKeyword }
    prefix     { TokenPrefixKeyword }
    postfix    { TokenPostfixKeyword }
    tinfix     { TokenInfixKeyword }
    bracketop  { TokenBracketOpKeyword }
    native     { TokenNativeKeyword }
    import     { TokenImportKeyword }
    '{'        { TokenCurlyOpen }
    '}'        { TokenCurlyClose }
    '['        { TokenSquareOpen }
    ']'        { TokenSquareClose }
    ';'        { TokenSemicolon }
    '_'        { TokenUnderscore }
    ident      { TokenIdent $$ }

%%

Program : {- empty -}                   { CProgramEnd }
	| ProgramStatement ';' Program      { CProgram $1 $3 }
    | FunctionDefinition Program { CProgram (CPSFuncDef $1) $2 }

FunctionDefinition : ident ident '(' Parameters ')' '{' BlockStatement '}'                             { CFuncDef $1 $2 $4 $7 }
		   | ident OperatorType '_' operator '_' '(' Parameters ')' '{' BlockStatement '}'     { COpOverloadDef $1 $2 $4 $7 $10 }
                   | native ident ident '(' Parameters ')'                                             { CFuncDefNative $2 $3 $5}
                   | ident bracketop '_' BracketType '_' '(' Parameters ')' '{' BlockStatement '}'     { CBracketOpOverloadDef $1 $4 $7 $10 } 

ExpressionList : {- empty -}                  { CListEmpty }
	       | FilledExpressionList   { CListNonEmpty $1 }

Expression : Constant                          { CEConst $1 }
	   | ident                                 { CEIdent $1 }
           | '(' Expression ')'                { CEBracketed $2 }
           | Expression '(' ExpressionList ')' { CEBracketOp Parentheses $1 $3 }
           | Expression '[' ExpressionList ']' { CEBracketOp Square $1 $3 }
           | Expression '{' ExpressionList '}' { CEBracketOp Curly $1 $3 }
           | operator Expression               { CEPrefixOp $1 $2 }
           | Expression operator               { CEPostfixOp $2 $1 }
           | Expression operator Expression    { CEInfixOp $2 $1 $3 }

Constant : string    { CCString $1 }
	 | integer   { CCInt $1 }
         | double    { CCDouble $1 }
         | char      { CCChar $1 }

FilledExpressionList : Expression ',' FilledExpressionList   { CMultiExpression $1 $3 }
		     | Expression                            { CFinalExpression $1 }

VariableDeclaration : ident ident        { CVarDec $1 $2 }

Statement : Expression              { CSExpression $1 }
	  | VariableDeclaration     { CSVarDec $1 }

Parameter : ident ident { CParam $1 $2 }
          | ident operator ident {% \s currentCol l tokenPositions -> let returnVal = (CPointerParam $1 $2 $3) in
                case $2 of
                    "*" -> return returnVal
                    "..." -> return returnVal
                    actualOperator -> let (sLine, sCol, eLine, eCol) = getTokenPositions returnVal tokenPositions 2 in
                            addNonFatalError
                            (NFE ("Only '*' of '...' can be used on a type to denote an operator. Received operator '" ++ actualOperator ++ "' instead.") s sLine sCol eLine eCol)
                            returnVal
            }

FilledParameters : Parameter ',' FilledParameters     { CMultiParam $1 $3 }
		 | Parameter                          { CFinalParam $1 }

Parameters : {- empty -}               { CPEmpty }
	   | FilledParameters    { CPParams $1 }

-- Removing from use
FilledBlock : Statement ';' FilledBlock         { CMultiStatement $1 $3 }
            | Statement ';'                   { CFinalStatement $1 }

BlockStatement : {- empty -}                           { CBlockEmpty }
               | FilledBlock    { CFilledBlock $1 }

OperatorType : prefix        { Prefix }
	     | postfix       { Postfix }
             | tinfix        { Infix }

BracketType : '(' ')' { Parentheses }
	        | '[' ']' { Square }
            | '{' '}' { Curly }

AliasDefinition : alias ident operator ident    {% \s currentCol l tokenPositions ->
                    case $3 of
                        "=" -> return $ CAliasDef $3 $2 $4 
                        _ -> let returnVal = (CAliasDef $3 $2 $4) in
                            let (sLine, sCol, eLine, eCol) = getTokenPositions returnVal tokenPositions 3 in
                                addNonFatalError
                                (NFE "A '=' operator must be used within an alias definition" s sLine sCol eLine eCol)
                                returnVal
                }
                | native alias ident operator nativecode {% \s currentCol l tokenPositions ->
                    case $4 of
                        "=" -> return $ CNativeAliasDef $4 $3 $5
                        actualOperator -> let returnVal = CNativeAliasDef $4 $3 $5 in
                            let (sLine, sCol, eLine, eCol) = getTokenPositions returnVal tokenPositions 4 in
                                addNonFatalError
                                (NFE ("A '=' operator must be used within an alias definition. Received operator '" ++ actualOperator ++ "' instead.") s sLine sCol eLine eCol)
                                returnVal
                }

ProgramStatement : VariableDeclaration     { CPSVarDec $1 }
		 | FunctionDefinition      { CPSFuncDef $1 }
                 | AliasDefinition         { CPSAliasDef $1 }
                 | ImportStatement         { CPSImportStatement $1 }

ImportStatement : import ModuleName            { CNSImport $2 }
                | native import ModuleName     { CNativeImport $3 }

-- ModuleName : ident                      { CModuleIdent $1 }
-- | ident operator ModuleName  { CPackage $1 $2 $3 }

ModuleName : ident { CModuleIdent $1 }
           | ident operator ModuleName {% \s currentCol l tokenPositions ->
                case $2 of
                    "." -> return $ CPackage $1 $2 $3
                    actualOperator -> let returnVal = (CPackage $1 $2 $3) in
                        let (sLine, sCol, eLine, eCol) = getTokenPositions returnVal tokenPositions 2 in
                        addNonFatalError 
                        (NFE ("package names must be separated by '.' operator. Received operator '" ++ actualOperator ++ "' instead.")  s sLine sCol eLine eCol)
                        returnVal
            }

{
getTokenPositions cstElem tokenPositions tokenNumber =
    (drop ((length tokenPositions) - (tokenLength cstElem)) tokenPositions) !! (tokenNumber - 2)

parseError :: Token -> Cs a
parseError t = getLineNumber `thenCs` \line  -> (failCs ((show t) ++ " at line " ++ (show line)))

thenCs :: Cs a -> (a -> Cs b) -> Cs b
m `thenCs` k = \s currentCol l tokenPositions-> (m s currentCol l tokenPositions) >>= (\x -> k x s currentCol l tokenPositions)

returnCs :: a -> Cs a
returnCs a = \s _ _ _ -> Valid empty a

failCs :: String -> Cs a
failCs err = \s currentCol l tokenPositions -> Error err (show (l, currentCol))
}

