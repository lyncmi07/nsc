module Com.NoSyn.Parser.ConcreteSyntaxTree where

data CConstant =
    CCString String
    | CCInt Int
    | CCDouble Float
    | CCChar Char
    deriving Show

data CFilledExpressionList =
    CMultiExpression CExpression CFilledExpressionList
    | CFinalExpression CExpression
    deriving Show

data CExpressionList =
    CListEmpty
    | CListNonEmpty CFilledExpressionList
    deriving Show

data CExpression =
    CEConst CConstant
    | CEIdent String
    | CEFuncCall String CExpressionList
    | CEPrefixOp String CExpression
    | CEPostfixOp String CExpression
    | CEInfixOp String CExpression CExpression
    deriving Show

data CVariableDeclaration = CVarDec String String

data CStatement = 
    CSExpression CExpression
    | CSVarDec CVariableDeclaration

data CParameter = CParam String String

data CFilledParameters =
    CMultiParam CParameter CFilledParameters
    | CFinalParam CParameter

data CParameters =
    CPEmpty
    | CPParams CFilledParameters

data CFilledBlock =
    CMultiStatement CStatement CStatement
    | CFinalStatement CStatement

data CBlockStatement =
    CBlockEmpty
    | CFilledBlock CFilledBlock

data CFunctionDefinition = CFuncDef String String CParameters CBlockStatement

data CAliasDefinition = CAliasDef String String

data CProgramStatement = 
    CPSVarDec CVariableDeclaration
    | CPSFuncDef CFunctionDefinition
    | CPSAliasDef CAliasDefinition

data CProgram =
    CProgramEnd
    | CProgram CProgramStatement CProgram