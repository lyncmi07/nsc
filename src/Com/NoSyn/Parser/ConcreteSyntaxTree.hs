module Com.NoSyn.Parser.ConcreteSyntaxTree where

import Com.NoSyn.Data.Operators

data CConstant =
    CCString String
    | CCInt Int
    | CCDouble Double
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
    deriving Show

data CStatement = 
    CSExpression CExpression
    | CSVarDec CVariableDeclaration
    deriving Show

data CParameter = CParam String String
    deriving Show

data CFilledParameters =
    CMultiParam CParameter CFilledParameters
    | CFinalParam CParameter
    deriving Show

data CParameters =
    CPEmpty
    | CPParams CFilledParameters
    deriving Show

data CFilledBlock =
    CMultiStatement CStatement CFilledBlock
    | CFinalStatement CStatement
    deriving Show

data CBlockStatement =
    CBlockEmpty
    | CFilledBlock CFilledBlock
    deriving Show

data CFunctionDefinition = 
    CFuncDef String String CParameters CBlockStatement
    | COpOverloadDef String OperatorType String CParameters CBlockStatement
    | CBracketOpOverloadDef String BracketType CParameters CBlockStatement
    deriving Show


data CAliasDefinition = CAliasDef String String
    deriving Show

data CProgramStatement = 
    CPSVarDec CVariableDeclaration
    | CPSFuncDef CFunctionDefinition
    | CPSAliasDef CAliasDefinition
    deriving Show

data CProgram =
    CProgramEnd
    | CProgram CProgramStatement CProgram
    deriving Show
