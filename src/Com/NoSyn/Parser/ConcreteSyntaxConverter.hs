module Com.NoSyn.Parser.ConcreteSyntaxConverter where

import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.If.Constant as IfConstant
import Com.NoSyn.Ast.Ifm1.Constant as Ifm1Constant
import Com.NoSyn.Ast.Ifm1.Expression
import Com.NoSyn.Ast.Ifm1.AliasDefinition
import Com.NoSyn.Ast.Ifm1.FunctionDefinition
import Com.NoSyn.Ast.If.Parameter as IfParameter
import Com.NoSyn.Ast.Ifm1.Parameter as Ifm1Parameter
import Com.NoSyn.Ast.Ifm1.Program
import Com.NoSyn.Ast.Ifm1.Statement
import Com.NoSyn.Ast.If.VariableDeclaration as IfVariableDeclaration
import Com.NoSyn.Ast.Ifm1.VariableDeclaration as Ifm1VariableDeclaration
import Com.NoSyn.Ast.Traits.Listable
import Com.NoSyn.Ast.If.ImportStatement as IfImportStatement
import Com.NoSyn.Ast.Ifm1.ImportStatement as Ifm1ImportStatement
import Com.NoSyn.Ast.Ifm1.PreProgram

import Com.NoSyn.Parser.ConcreteSyntaxTree

import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Data.Operators

convertConstant :: CConstant -> CompilerStatus Ifm1Constant.Constant
convertConstant (CCString a) = return $ Ifm1Constant.IfConstant $ CString a
convertConstant (CCInt a) = return $ Ifm1Constant.IfConstant $ CInt a
convertConstant (CCDouble a) = return $ Ifm1Constant.IfConstant $ CDouble a
convertConstant (CCChar a) = return $ Ifm1Constant.IfConstant $ CChar a

convertFilledExpressionList :: CFilledExpressionList -> CompilerStatus [Expression]
convertFilledExpressionList (CMultiExpression x xs) = do
    n <- convertExpression x
    m <- convertFilledExpressionList xs
    return $ n:m
convertFilledExpressionList (CFinalExpression x) = sequence $ [convertExpression x]

convertExpressionList :: CExpressionList -> CompilerStatus [Expression]
convertExpressionList CListEmpty = return []
convertExpressionList (CListNonEmpty a) = convertFilledExpressionList a

convertExpression :: CExpression -> CompilerStatus Expression
convertExpression (CEConst a) = do
    n <- convertConstant a
    return $ EConst n
convertExpression (CEIdent a) = return $ EIdent a
convertExpression (CEBracketed a) = convertExpression a
convertExpression (CEBracketOp bracketType a b) = do
    n <- convertExpression a
    m <- convertExpressionList b
    return $ EBrackets bracketType (n:m)
convertExpression prefixOp@(CEPrefixOp a b) =
    case b of
        (CEInfixOp _ _ _) -> convertExpression $ reorderPrefixOperator prefixOp
        _ -> do
            n <- convertExpression b
            return $ EOp Prefix a [n]
convertExpression (CEPostfixOp a b) = do
    n <- convertExpression b
    return $ EOp Postfix a [n]
convertExpression reversedInfix@(CEInfixOp _ _ _)= do
    n <- convertExpression a
    m <- convertExpression b
    return $ EOp Infix o (n:m:[])
    where
        (CEInfixOp o a b) = reverseOperatorOrder reversedInfix

reorderPrefixOperator :: CExpression -> CExpression
reorderPrefixOperator (CEPrefixOp a b) = reorderPrefixOperator' a b

reorderPrefixOperator' :: String -> CExpression -> CExpression
reorderPrefixOperator' prefixOp (CEInfixOp a b c) =
    (CEInfixOp a (reorderPrefixOperator' prefixOp b) c)
reorderPrefixOperator' prefixOp nonInfixExp = (CEPrefixOp prefixOp nonInfixExp)


reverseOperatorOrder :: CExpression -> CExpression
reverseOperatorOrder a =
    let flattenedExpressions = flattenInfixExpression a in
    createLeftOrderedInfixExpression flattenedExpressions

flattenInfixExpression :: CExpression -> [(String, CExpression)]
flattenInfixExpression (CEInfixOp o a b) =
    (o, a):(flattenInfixExpression b)
flattenInfixExpression a = [("", a)]

createLeftOrderedInfixExpression (x:xs) = createLeftOrderedInfixExpression' x xs
createLeftOrderedInfixExpression' :: (String, CExpression) -> [(String, CExpression)] -> CExpression
createLeftOrderedInfixExpression' (o, a) ((_, b):[]) = CEInfixOp o a b
createLeftOrderedInfixExpression' (o, a) ((on, b):xs) =
    createLeftOrderedInfixExpression' (on, CEInfixOp o a b) xs

convertVariableDeclaration :: CVariableDeclaration -> CompilerStatus Ifm1VariableDeclaration.VariableDeclaration
convertVariableDeclaration (CVarDec a b) = return $ Ifm1VariableDeclaration.IfVariableDeclaration $ VDec a b

convertStatement :: CStatement -> CompilerStatus Statement
convertStatement (CSExpression a) = do
    n <- convertExpression a
    return $ SExpression n
convertStatement (CSVarDec a) = do
    n <- convertVariableDeclaration a
    return $ SVarDec n


convertParameter :: CParameter -> CompilerStatus Ifm1Parameter.Parameter
convertParameter (CParam a b) = return $ Ifm1Parameter.IfParameter $ PConst a b
convertParameter (CPointerParam a op b)
    | op == "*" = return $ Ifm1Parameter.IfParameter $ PPointer a b
    | op == "..." = return $ Ifm1Parameter.IfParameter $ PVariadic a b
    | otherwise = Error "Only '*' or '...' can be used on a type to denote an operator" (show (CPointerParam a op b))

convertFilledParameters :: CFilledParameters -> CompilerStatus [Ifm1Parameter.Parameter]
convertFilledParameters (CMultiParam x xs) = do
    n <- convertParameter x
    m <- convertFilledParameters xs
    return $ n:m
convertFilledParameters (CFinalParam a) = sequence $ [convertParameter a]

convertParameters :: CParameters -> CompilerStatus Ifm1Parameter.Parameters
convertParameters CPEmpty = return $ StandardBlock []
convertParameters (CPParams a) = do
    n <- convertFilledParameters a
    return $ StandardBlock n

convertFilledBlock :: CFilledBlock -> CompilerStatus [Statement]
convertFilledBlock (CMultiStatement x xs) = do
    n <- convertStatement x
    m <- convertFilledBlock xs
    return $ n:m
convertFilledBlock (CFinalStatement x) = sequence [convertStatement x]

convertBlockStatement :: CBlockStatement -> CompilerStatus BlockStatement
convertBlockStatement CBlockEmpty = return $ SequentialBlock []
convertBlockStatement (CFilledBlock a) = do
    n <- convertFilledBlock a
    return $ SequentialBlock n

convertFunctionDefinition :: CFunctionDefinition -> CompilerStatus FunctionDefinition
convertFunctionDefinition (CFuncDefNative b a c) = do
    n <- convertParameters c
    return $ FDNative a b n
convertFunctionDefinition (CFuncDef b a c d) = do
    n <- convertParameters c
    m <- convertBlockStatement d
    return $ FDNoSyn a b n m
convertFunctionDefinition (COpOverloadDef a b c d e) = do
    n <- convertParameters d
    m <- convertBlockStatement e
    return $ FDOperatorOverload b c a n m
convertFunctionDefinition (CBracketOpOverloadDef a b c d) = do
    n <- convertParameters c
    m <- convertBlockStatement d
    return $ FDBracketOverload b a n m

convertAliasDefinition :: CAliasDefinition -> CompilerStatus AliasDefinition
convertAliasDefinition (CAliasDef o a b)
    | o == "=" = return $ ADNoSyn a b
    | otherwise = Error "alias statement must assign using '=' symbol" (show (CAliasDef o a b))
convertAliasDefinition (CNativeAliasDef o a b)
    | o == "=" = return $ ADNative a b
    | otherwise = Error "alias statement must assign using '=' symbol" (show (CNativeAliasDef o a b))

convertProgramStatement :: CProgramStatement -> CompilerStatus ProgramStmt
convertProgramStatement (CPSVarDec a) = do
    n <- convertVariableDeclaration a
    return $ PSVarDec n
convertProgramStatement (CPSFuncDef a) = do
    n <- convertFunctionDefinition a
    return $ PSFuncDef n
convertProgramStatement (CPSAliasDef a) = do
    n <- convertAliasDefinition a
    return $ PSAliasDef n
convertProgramStatement (CPSImportStatement a) = 
    Error "COMPILER ERROR: Import statements should not be present in this context" (show (CPSImportStatement a))

convertProgram :: CProgram -> CompilerStatus PreProgram
convertProgram program = do
    convertedImportStatements <- convertImportStatements importStatements
    convertedProgramStatements <- convertProgramStatements programStatements
    return $ PreProgram convertedImportStatements convertedProgramStatements
    where
        flattenProgramStatements CProgramEnd = []
        flattenProgramStatements (CProgram x xs) = x:(flattenProgramStatements xs)
        flattenedStatements = flattenProgramStatements program
        importStatements = filter importStatementPredicate flattenedStatements
        programStatements = filter (not.importStatementPredicate) flattenedStatements
        importStatementPredicate x = case x of
            (CPSImportStatement _) -> True
            _ -> False

convertImportStatements :: [CProgramStatement] -> CompilerStatus Ifm1ImportStatement.ImportStatements
convertImportStatements importStatements = do
    ifm1ImportStatements <- sequence $ map convertImportStatement importStatements
    return $ StandardBlock ifm1ImportStatements

convertProgramStatements :: [CProgramStatement] -> CompilerStatus Program
convertProgramStatements programStatements = do
    ifm1ProgramStatements <- sequence $ map convertProgramStatement programStatements
    return $ StandardBlock ifm1ProgramStatements

convertImportStatement :: CProgramStatement -> CompilerStatus Ifm1ImportStatement.ImportStatement
convertImportStatement (CPSImportStatement a) = convertImportStatement' a
convertImportStatement a = Error "COMPILER ERROR: Only import statements should be present in this context" (show a)

convertImportStatement' (CNSImport a) = do
    n <- convertModuleName a
    return $ Ifm1ImportStatement.IfImportStatement $ NSImport n
convertImportStatement' (CNativeImport a) = do
    n <- convertModuleName a
    return $ Ifm1ImportStatement.IfImportStatement $ NativeImport n

convertModuleName :: CModuleName -> CompilerStatus [String]
convertModuleName (CModuleIdent moduleName) = return [moduleName]
convertModuleName (CPackage parentPackage operator childPackage)
    | (operator == ".") = do
        rest <- convertModuleName childPackage
        return $ parentPackage:rest
    | otherwise = Error "package names are separated by '.' operator" (show (CPackage parentPackage operator childPackage))
