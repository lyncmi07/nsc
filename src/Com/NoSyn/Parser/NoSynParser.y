{
module Com.NoSyn.Parser.NoSynParser where

import Com.NoSyn.Data.Types
import Com.NoSyn.Parser.ConcreteSyntaxTree
import Com.NoSyn.Parser.Token
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    string     { TokenString $$ }
    integer    { TokenInt $$ }
    double     { TokenDouble $$ }
    char       { TokenChar $$ }
    operator   { TokenOperator $$ }
    ','        { TokenComma }
    '('        { TokenParameterOpen }
    ')'        { TokenParameterClose }
    '='        { TokenEquals }
    alias      { TokenAliasKeyword }
    '{'        { TokenCurlyOpen }
    '}'        { TokenCurlyClose }
    ';'        { TokenSemicolon }
    empty      { TokenEmpty }
    ident      { TokenIdent }

%%

Constant : string    { CCString $1 }
	 | integer   { CCInt $1 }
         | double    { CCDouble $1 }
         | char      { CCChar $1 }

FilledExpressionList : Expression ',' FilledExpressionList   { CMultiExpression $1 $3 }
		     | Expression                            { CFinalExpression $1 }

ExpressionList : empty                  { CListEmpty }
	       | FilledExpressionList   { CListNonEmpty $1 }

Expression : Constant                         { CEConst $1 }
	   | ident                            { CEIdent $1 }
           | ident '(' ExpressionList ')'     { CEFuncCall $1 $3 }
           | operator Expression              { CEPrefixOp $1 $2 }
           | Expression operator              { CEPostfixOp $2 $1 }
           | Expression operator Expression   { CEInfixOp $2 $1 $3 }

VariableDeclaration : ident ident        { CVarDec $1 $2 }

Statement : Expression              { CSExpression $1 }
	  | VariableDeclaration     { CSVarDec $1 }

Parameter : ident ident { Param $1 $2 }

FilledParameters : Parameter ',' FilledParameters     { CMultiParam $1 $3 }
		 | Parameter                          { CFinalParam $1 }

Parameters : empty               { PEmpty }
	   | FilledParameters    { PParams $1 }

FilledBlock : Statement ';' FilledBlock         { CMultiStatement $1 $3 }
	    | Statement                         { CFinalStatement $1 }

BlockStatement : empty          { CBlockEmpty }
	       | FilledBlock    { CFilledBlock $1 }

FunctionDefinition : ident ident '(' Parameters ')' '{' BlockStatement '}' { CFuncDef $1 $2 $4 $7 }

AliasDefinition : alias ident '=' ident    { CAliasDef $2 $4 }

ProgramStatement : VariableDeclaration     { CPSVarDec $1 }
		 | FunctionDefinition      { CPSFuncDef $1 }
                 | AliasDefinition         { CPSAliasDef $1 }

Program : empty                             { CProgramEnd }
	| ProgramStatement ';' Program      { CProgram $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parser Error"

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isWhiteSpace c = lexer cs

}
