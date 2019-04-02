module Com.NoSyn.Parser.ConcreteSyntaxConverter where

import Com.NoSyt.Ast.If.Constant as IfConstant
import Com.NoSyn.Ast.Ifm1.Constant as Ifm1Constant
import Com.NoSyn.Ast.Ifm1.Expression
import Com.NoSyn.Ast.Ifm1.FunctionDefinition
import Com.NoSyn.Ast.If.Parameter as IfParameter
import Com.NoSyn.Ast.Ifm1.Parameter as Ifm1Parameter
import Com.NoSyn.Ast.Ifm1.Program
import Com.NoSyn.Ast.Ifm1.Statement
import Com.NoSyn.Ast.If.VariableDeclaration as IfVariableDeclaration
import Com.NoSyn.Ast.Ifm1.VariableDeclaration as Ifm1VariableDeclaration

import Com.NoSyn.Parser.ConcreteSyntaxTree

import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Data.Operators
import Data.List.Utils

convertConstant :: CConstant -> CompilerStatus Ifm1Constant.Constant
convertConstant (CCString a) = Error "Currently unsupported language syntax"
convertConstant (CCInt a) = return $ Ifm1Constant.IfConstant $ CInt a
convertConstant (CDouble a) = return $ Ifm1Constant.IfConstant $ CDouble a
convertConstant (CChar a) = return $ Ifm1Constant.IfConstant $ CChar a

convertFilledExpressionList :: CFilledExpressionList -> CompilerStatus [Expression]
convertFilledExpressionList (CFilledExpressionList x xs) = do
    n <- convertExpression x
    m <- convertExpressionList xs
    return $ n:m

convertExpressionList :: CExpressionList -> CompilerStatus [Expression]
convertExpressionList CListEmpty = return []
convertExpressionList CListNonEmpty a = convertFilledExpressionList a

convertExpression :: CExpression -> CompilerStatus Expression
convertExpression (CEConst a) = do
    n <- convertConstant a
    return $ EConst n
convertExpression (CEIdent a) = return $ EIdent a
convertExpression (CEFuncCall a b) = do
    n <- convertExpressionList b
    return $ EFuncCall a n
convertExpression (CEPrefixOp a b) = do
    n <- convertExpression b
    return $ EOp a Prefix n
convertExpression (CEPostfixOp a b) = do
    n <- convertExpression b
    return $ EOp a Postfix n
convertExpression (CEInfixOp a b) = do
    n <- convertExpression b
    return $ EOp a Infix n

convertVariableDeclaration :: CVariableDeclaration -> CompilerStatus Ifm1VariableDeclaration.VariableDeclaration
convertVariableDeclaration (CVarDec a b) = return $ Ifm1VariableDeclaration.IfVarDec $ VDec a b

convertStatement :: CStatement -> CompilerStatus Statement
convertStatement (CSExpression a) = do
    n <- convertExpression a
    return $ SExpression n
convertStatement (CSVarDec a) = do
    n <- convertVariableDeclaration a
    return $ SVarDec n


convertParameter :: CParameter -> CompilerStatus Ifm1Parameter.Parameter
convertParameter (CParameter a b)
    | (a `endswith` "*") = return $ Ifm1Parameter.IfParameter $ PPointer (dropEnd 1 a) b
    | otherwise = return $ Ifm1Parameter.IfParameter $ PConst a b
    where
        dropEnd x = (reverse.(drop x).reverse)

convertFilledParameter :: CFilledParameter -> CompilerStatus [Ifm1Parameter.Parameter]
convertFilledParameter (CMultiParam x xs) = do
    n <- convertParameter n
    m <- convertFilledParameter xs
    return $ n:m

convertParameters :: CParameters -> CompilerStatus Ifm1Parameter.Parameters
convertParameters CPEmpty = return $ StandardBlock []
convertParameters (CPParams a) = do
    n <- convertFilledParameters a
    return $ StandardBlock n

convertFilledBlock :: CFilledBlock -> CompilerStatus [Statement]
convertFilledBlock (CMultiParam x xs) = do
    n <- convertStatement x
    m <- convertFilledBlock xs
    return $ x:xs
convertFilledBlock (CFinalParam x) = sequence [convertStatement x]

convertBlockStatement :: CBlockStatement -> CompilerStatus BlockStatement
convertBlockStatement CBlockEmpty = return $ SequentialBlock []
convertBlockStatement (CFilledBlock a) = do
    n <- convertFilledBlock
    return $ SequentialBlock n

--this requires some lexing
--convertFunctionDefinition CFunctionDefinition -> CompilerStatus FunctionDefinition

convertAliasDefinition :: CAliasDefinition -> CompilerStatus ProgramStatement
convertAliasDefinition (CAliasDef a b) = return $ PSAliasDef a b

convertProgramStatement :: CProgramStatement -> CompilerStatus ProgramStatement
convertProgramStatement (CPSVarDec a) = do
    n <- convertVariableDeclaration a
    return $ PSVarDec n
convertProgramStatement (CPSFuncDef a) = do
    n <- convertFunctionDefinition a)
    return $ PSFuncDef n
convertProgramStatement (CPSAliasDef a) = do
    n <- convertAliasDefinition a
    return $ PSAliasDef n

convertProgram :: CProgram -> CompilerStatus Program
convertProgram CProgramEnd = return $ StandardBlock []
convertProgram (CProgram x xs) = do
    n <- convertProgramStatement x
    m <- convertProgram m
    return $ StandardBlock $ n:m
