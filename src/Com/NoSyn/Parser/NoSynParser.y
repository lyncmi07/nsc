{
module Com.NoSyn.Parser.NoSynParser where
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

Constant : string    { CString $1 }
	 | integer   { CInt $1 }
         | double    { CDouble $1 }
         | char      { CChar $1 }

FilledExpressionList : Expression ',' FilledExpressionList   { MultiExpression $1 $3 }
		     | Expression                            { FinalExpression $1 }

ExpressionList : empty                  { ListEmpty }
	       | FilledExpressionList   { ListNonEmpty $1 }

Expression : Constant                         { EConst $1 }
	   | ident                            { EIdent $1 }
           | ident '(' ExpressionList ')'     { EFuncCall $1 $3 }
           | operator Expression              { EPrefixOp $1 $2 }
           | Expression operator              { EPostfixOp $2 $1 }
           | Expression operator Expression   { EInfixOp $2 $1 $3 }

VariableDeclaration : ident ident        { VarDec $1 $2 }

Statement : Expression              { SExpression $1 }
	  | VariableDeclaration     { SVarDec $1 }

Parameter : ident ident { Param $1 $2 }

FilledParameters : Parameter ',' FilledParameters     { MultiParam $1 $3 }
		 | Parameter                          { FinalParam $1 }

Parameters : empty               { PEmpty }
	   | FilledParameters    { PParams $1 }

FilledBlock : Statement ';' FilledBlock         { MultiStatement $1 $3 }
	    | Statement                         { FinalStatement $1 }

BlockStatement : empty          { BlockEmpty }
	       | FilledBlock    { FilledBlock $1 }

FunctionDefinition : ident ident '(' Parameters ')' '{' BlockStatement '}' { FuncDef $1 $2 $4 $7 }

AliasDefinition : alias ident '=' ident    { AliasDef $2 $4 }

ProgramStatement : VariableDeclaration     { PSVarDec $1 }
		 | FunctionDefinition      { PSFuncDef $1 }
                 | AliasDefinition         { PSAliasDef $1 }

Program : empty                             { ProgramEnd }
	| ProgramStatement ';' Program      { Program $1 $3 }
