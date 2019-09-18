module Com.NoSyn.Parser.ConcreteSyntaxTree where

import Com.NoSyn.Data.Operators

class TokenLength a where
  tokenLength :: a -> Int

data CConstant =
    CCString String
    | CCInt Int
    | CCDouble Double
    | CCChar Char
    deriving Show
instance TokenLength CConstant where
    tokenLength _ = 1

data CFilledExpressionList =
    CMultiExpression CExpression CFilledExpressionList
    | CFinalExpression CExpression
    deriving Show
instance TokenLength CFilledExpressionList where
    tokenLength (CMultiExpression a b) = (tokenLength a) + (tokenLength b)
    tokenLength (CFinalExpression a) = tokenLength a

data CExpressionList =
    CListEmpty
    | CListNonEmpty CFilledExpressionList
    deriving Show
instance TokenLength CExpressionList where
    tokenLength CListEmpty = 0
    tokenLength (CListNonEmpty a) = (tokenLength a)

data CExpression =
    CEConst CConstant
    | CEIdent String
    | CEPrefixOp String CExpression
    | CEPostfixOp String CExpression
    | CEInfixOp String CExpression CExpression
    | CEBracketOp BracketType CExpression CExpressionList
    | CEBracketed CExpression
    deriving Show
instance TokenLength CExpression where
    tokenLength (CEConst a) = tokenLength a
    tokenLength (CEIdent _) = 1
    tokenLength (CEPrefixOp _ a) = 1 + (tokenLength a)
    tokenLength (CEPostfixOp _ a) = 1 + (tokenLength a)
    tokenLength (CEInfixOp _ a b) = 1 + (tokenLength a) + (tokenLength b)
    
    

data CVariableDeclaration = CVarDec String String
    deriving Show
instance TokenLength CVariableDeclaration where
    tokenLength _ = 2

data CStatement = 
    CSExpression CExpression
    | CSVarDec CVariableDeclaration
    deriving Show
instance TokenLength CStatement where
    tokenLength (CSExpression a) = tokenLength a
    tokenLength (CSVarDec a) = tokenLength a

data CParameter =
    CParam String String
    | CPointerParam String String String
    deriving Show
instance TokenLength CParameter where
    tokenLength (CParam _ _) = 2
    tokenLength (CPointerParam _ _ _) = 3

data CFilledParameters =
    CMultiParam CParameter CFilledParameters
    | CFinalParam CParameter
    deriving Show
instance TokenLength CFilledParameters where
    tokenLength (CMultiParam a b) = (tokenLength a) + 1 + (tokenLength b)
    tokenLength (CFinalParam a) = tokenLength a

data CParameters =
    CPEmpty
    | CPParams CFilledParameters
    deriving Show
instance TokenLength CParameters where
    tokenLength CPEmpty = 0
    tokenLength (CPParams a) = (tokenLength a)

data CFilledBlock =
    CMultiStatement CStatement CFilledBlock
    | CFinalStatement CStatement
    deriving Show
instance TokenLength CFilledBlock where
    tokenLength (CMultiStatement a b) = (tokenLength a) + 1 + (tokenLength b)
    tokenLength (CFinalStatement a) = (tokenLength a) + 1

data CBlockStatement =
    CBlockEmpty
    | CFilledBlock CFilledBlock
    deriving Show
instance TokenLength CBlockStatement where
    tokenLength CBlockEmpty = 0
    tokenLength (CFilledBlock a) = tokenLength a

data CFunctionDefinition = 
    CFuncDefNative String String CParameters
    | CFuncDef String String CParameters CBlockStatement
    | COpOverloadDef String OperatorType String CParameters CBlockStatement
    | CBracketOpOverloadDef String BracketType CParameters CBlockStatement
    deriving Show
instance TokenLength CFunctionDefinition where
    tokenLength (CFuncDefNative _ _ a) = 5 + (tokenLength a)
    tokenLength (CFuncDef _ _ a b) = 6 + (tokenLength a) + (tokenLength b)
    tokenLength (COpOverloadDef _ _ _ a b) = 9 + (tokenLength a) + (tokenLength b)
    tokenLength (CBracketOpOverloadDef _ _ a b) = 9 + (tokenLength a) + (tokenLength b)


data CAliasDefinition = CAliasDef String String String
    | CNativeAliasDef String String String
    deriving Show
instance TokenLength CAliasDefinition where
    tokenLength (CAliasDef _ _ _) = 4
    tokenLength (CNativeAliasDef _ _ _) = 5

data CProgramStatement = 
    CPSVarDec CVariableDeclaration
    | CPSFuncDef CFunctionDefinition
    | CPSAliasDef CAliasDefinition
    | CPSImportStatement CImportStatement
    deriving Show
instance TokenLength CProgramStatement where
    tokenLength (CPSVarDec a) = tokenLength a
    tokenLength (CPSFuncDef a) = tokenLength a
    tokenLength (CPSAliasDef a) = tokenLength a
    tokenLength (CPSImportStatement a) = tokenLength a

data CProgram =
    CProgramEnd
    | CProgram CProgramStatement CProgram
    deriving Show
instance TokenLength CProgram where
    tokenLength CProgramEnd = 0
    tokenLength (CProgram a b) = (tokenLength a) + 1 + (tokenLength b)

data CImportStatement =
    CNSImport CModuleName
    | CNativeImport CModuleName
    deriving Show
instance TokenLength CImportStatement where
    tokenLength (CNSImport a) = 1 + (tokenLength a)
    tokenLength (CNativeImport a) = 1 + (tokenLength a)

data CModuleName =
    CModuleIdent String
    | CPackage String String CModuleName
    deriving Show
instance TokenLength CModuleName where
    tokenLength (CModuleIdent _) = 1
    tokenLength (CPackage _ _ a) = 2 + (tokenLength a)
