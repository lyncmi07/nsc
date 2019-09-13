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
          | ident operator ident {% \s l ->
                case $2 of
                    "*" -> return $ CPointerParam $1 $2 $3
                    "..." -> return $ CPointerParam $1 $2 $3
                    _ -> addNonFatalError
                        (NFE "Only '*' of '...' can be used on a type to denote an operator" s l)
                        (CPointerParam $1 $2 $3)
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

AliasDefinition : alias ident operator ident    {% \s l ->
                    case $3 of
                        "=" -> return $ CAliasDef $3 $2 $4 
                        _ -> addNonFatalError
                            (NFE "A '=' operator must be used within an alias definition" s l)
                            (CAliasDef $3 $2 $4)
                }
                | native alias ident operator nativecode {% \s l ->
                    case $3 of
                        "=" -> return $ CAliasDef $4 $3 $5
                        _ -> addNonFatalError
                            (NFE "A '=' operator must be used within an alias definition" s l)
                            (CAliasDef $4 $3 $5)
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
           | ident operator ModuleName {% \s l ->
                case $2 of
                    "." -> return $ CPackage $1 $2 $3
                    _ -> addNonFatalError 
                        (NFE "package names are separated by '.' operator"  s l)
                        (CPackage $1 $2 $3)

            }

{
-- parseError (x:_) = Error "Parse error" (show x)

parseError :: Token -> Cs a
parseError t = getLineNo `thenCs` \line -> (failCs ((show t) ++ " at line " ++ (show line)))

thenCs :: Cs a -> (a -> Cs b) -> Cs b
m `thenCs` k = \s l -> (m s l) >>= (\x -> k x s l)

returnCs :: a -> Cs a
returnCs a = \s l -> Valid empty a

failCs :: String -> Cs a
failCs err = \s l -> Error err s
}

