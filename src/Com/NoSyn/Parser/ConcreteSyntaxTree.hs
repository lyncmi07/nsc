module Com.NoSyn.Parser.ConcreteSyntaxTree where

import Prelude hiding (getContents)
import Com.NoSyn.Parser.TokenLength
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Data.Operators

type SPCConstant = SourcePosition CConstant 
data CConstant =
    CCString String
    | CCInt Int
    | CCDouble Double
    | CCChar Char
    deriving Show
instance TokenLength CConstant where
    tokenLength _ = 1

type SPCFilledExpressionList = SourcePosition CFilledExpressionList 
data CFilledExpressionList =
    CMultiExpression SPCExpression SPCFilledExpressionList
    | CFinalExpression SPCExpression
    deriving Show
instance TokenLength CFilledExpressionList where
    tokenLength (CMultiExpression a b) = (tokenLength $ getContents  a) + (tokenLength $ getContents b)
    tokenLength (CFinalExpression a) = tokenLength $ getContents  a

type SPCExpressionList = SourcePosition CExpressionList 
data CExpressionList =
    CListEmpty
    | CListNonEmpty SPCFilledExpressionList
    deriving Show
instance TokenLength CExpressionList where
    tokenLength CListEmpty = 0
    tokenLength (CListNonEmpty a) = (tokenLength $ getContents a)

type SPCExpression = SourcePosition CExpression 
data CExpression =
    CEConst SPCConstant
    | CEIdent String
    | CEPrefixOp String SPCExpression
    | CEPostfixOp String SPCExpression
    | CEInfixOp String SPCExpression SPCExpression
    | CEBracketOp BracketType SPCExpression SPCExpressionList
    | CEBracketed SPCExpression
    deriving Show
instance TokenLength CExpression where
    tokenLength (CEConst a) = tokenLength $ getContents a
    tokenLength (CEIdent _) = 1
    tokenLength (CEPrefixOp _ a) = 1 + (tokenLength $ getContents a)
    tokenLength (CEPostfixOp _ a) = 1 + (tokenLength $ getContents a)
    tokenLength (CEInfixOp _ a b) = 1 + (tokenLength $ getContents a) + (tokenLength $ getContents b)
    tokenLength (CEBracketOp a b c) = 2 + (tokenLength $ getContents b) + (tokenLength $ getContents c)
    tokenLength (CEBracketed a) = 2 + (tokenLength $ getContents a)
    
    

type SPCVariableDeclaration = SourcePosition CVariableDeclaration 
data CVariableDeclaration = CVarDec String String
    deriving Show
instance TokenLength CVariableDeclaration where
    tokenLength _ = 2

type SPCStatement = SourcePosition CStatement 
data CStatement = 
    CSExpression SPCExpression
    | CSVarDec SPCVariableDeclaration
    deriving Show
instance TokenLength CStatement where
    tokenLength (CSExpression a) = tokenLength $ getContents a
    tokenLength (CSVarDec a) = tokenLength $ getContents a

type SPCParameter = SourcePosition CParameter 
data CParameter =
    CParam String String
    | CPointerParam String String
    | CVariadicParam String String
    deriving Show
instance TokenLength CParameter where
    tokenLength (CParam _ _) = 2
    tokenLength (CPointerParam _ _) = 3
    tokenLength (CVariadicParam _ _) = 3

type SPCFilledParameters = SourcePosition CFilledParameters 
data CFilledParameters =
    CMultiParam SPCParameter SPCFilledParameters
    | CFinalParam SPCParameter
    deriving Show
instance TokenLength CFilledParameters where
    tokenLength (CMultiParam a b) = (tokenLength $ getContents a) + 1 + (tokenLength $ getContents b)
    tokenLength (CFinalParam a) = tokenLength $ getContents a

type SPCParameters = SourcePosition CParameters 
data CParameters =
    CPEmpty
    | CPParams SPCFilledParameters
    deriving Show
instance TokenLength CParameters where
    tokenLength CPEmpty = 0
    tokenLength (CPParams a) = (tokenLength $ getContents a)

type SPCFilledBlock = SourcePosition CFilledBlock 
data CFilledBlock =
    CMultiStatement SPCStatement SPCFilledBlock
    | CFinalStatement SPCStatement
    deriving Show
instance TokenLength CFilledBlock where
    tokenLength (CMultiStatement a b) = (tokenLength $ getContents a) + 1 + (tokenLength $ getContents b)
    tokenLength (CFinalStatement a) = (tokenLength $ getContents a) + 1

type SPCBlockStatement = SourcePosition CBlockStatement 
data CBlockStatement =
    CBlockEmpty
    | CFilledBlock SPCFilledBlock
    deriving Show
instance TokenLength CBlockStatement where
    tokenLength CBlockEmpty = 0
    tokenLength (CFilledBlock a) = tokenLength $ getContents a

type SPCFunctionDefinition = SourcePosition CFunctionDefinition 
data CFunctionDefinition = 
    CFuncDefNative String String SPCParameters
    | CFuncDef String String SPCParameters SPCBlockStatement
    | COpOverloadDef String OperatorType String SPCParameters SPCBlockStatement
    | CBracketOpOverloadDef String BracketType SPCParameters SPCBlockStatement
    deriving Show
instance TokenLength CFunctionDefinition where
    tokenLength (CFuncDefNative _ _ a) = 5 + (tokenLength $ getContents a)
    tokenLength (CFuncDef _ _ a b) = 6 + (tokenLength $ getContents a) + (tokenLength $ getContents b)
    tokenLength (COpOverloadDef _ _ _ a b) = 9 + (tokenLength $ getContents a) + (tokenLength $ getContents b)
    tokenLength (CBracketOpOverloadDef _ _ a b) = 9 + (tokenLength $ getContents a) + (tokenLength $ getContents b)


type SPCAliasDefinition = SourcePosition CAliasDefinition 
data CAliasDefinition = CAliasDef String String
    | CNativeAliasDef String String
    deriving Show
instance TokenLength CAliasDefinition where
    tokenLength (CAliasDef _ _) = 4
    tokenLength (CNativeAliasDef _ _) = 5

type SPCProgramStatement = SourcePosition CProgramStatement 
data CProgramStatement = 
    CPSVarDec SPCVariableDeclaration
    | CPSFuncDef SPCFunctionDefinition
    | CPSAliasDef SPCAliasDefinition
    | CPSImportStatement SPCImportStatement
    deriving Show
instance TokenLength CProgramStatement where
    tokenLength (CPSVarDec a) = tokenLength $ getContents a
    tokenLength (CPSFuncDef a) = tokenLength $ getContents a
    tokenLength (CPSAliasDef a) = tokenLength $ getContents a
    tokenLength (CPSImportStatement a) = tokenLength $ getContents a

type SPCProgram = SourcePosition CProgram 
data CProgram =
    CProgramEnd
    | CProgram SPCProgramStatement SPCProgram
    deriving Show
instance TokenLength CProgram where
    tokenLength CProgramEnd = 0
    tokenLength (CProgram a b) = (tokenLength $ getContents a) + 1 + (tokenLength $ getContents b)

type SPCImportStatement = SourcePosition CImportStatement 
data CImportStatement =
    CNSImport SPCModuleName
    | CNativeImport SPCModuleName
    deriving Show
instance TokenLength CImportStatement where
    tokenLength (CNSImport a) = 1 + (tokenLength $ getContents a)
    tokenLength (CNativeImport a) = 1 + (tokenLength $ getContents a)

type SPCModuleName = SourcePosition CModuleName 
data CModuleName =
    CModuleIdent String
    | CPackage String SPCModuleName
    deriving Show
instance TokenLength CModuleName where
    tokenLength (CModuleIdent _) = 1
    tokenLength (CPackage _ a) = 2 + (tokenLength $ getContents a)
