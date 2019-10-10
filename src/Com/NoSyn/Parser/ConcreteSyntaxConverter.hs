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
import Prelude hiding (getContents)
import Control.Monad.Trans.Class

import Com.NoSyn.Parser.ConcreteSyntaxTree

import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Data.Operators

convertConstant :: SPCConstant -> CompilerStatusT SourcePosition Ifm1Constant.Constant
convertConstant (CCString a) = return $ Ifm1Constant.IfConstant $ CString a
convertConstant (CCInt a) = return $ Ifm1Constant.IfConstant $ CInt a
convertConstant (CCDouble a) = return $ Ifm1Constant.IfConstant $ CDouble a
convertConstant (CCChar a) = return $ Ifm1Constant.IfConstant $ CChar a

convertFilledExpressionList :: SPCFilledExpressionList -> CompilerStatusT SourcePosition [Expression]
convertFilledExpressionList (CMultiExpression x xs) = do
    n <- convertExpression x
    m <- convertFilledExpressionList xs
    return $ n:m
convertFilledExpressionList (CFinalExpression x) = sequence $ [convertExpression x]

convertExpressionList :: SPCExpressionList -> CompilerStatusT SourcePosition [Expression]
convertExpressionList CListEmpty = return []
convertExpressionList (CListNonEmpty a) = convertFilledExpressionList a

convertExpression :: SPCExpression -> CompilerStatusT SourcePosition Expression
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

reorderPrefixOperator :: SPCExpression -> SourcePosition CExpression
reorderPrefixOperator (CEPrefixOp a b) = reorderPrefixOperator' a b

reorderPrefixOperator' :: String -> CExpression -> CExpression
reorderPrefixOperator' prefixOp (CEInfixOp a b c) =
    (CEInfixOp a (reorderPrefixOperator' prefixOp b) c)
reorderPrefixOperator' prefixOp nonInfixExp = (CEPrefixOp prefixOp nonInfixExp)


reverseOperatorOrder :: SPCExpression -> SourcePosition CExpression
reverseOperatorOrder a =
    let flattenedExpressions = flattenInfixExpression a in
    createLeftOrderedInfixExpression flattenedExpressions

flattenInfixExpression :: SPCExpression -> SourcePosition [(String, CExpression)]
flattenInfixExpression (CEInfixOp o a b) =
    (o, a):(flattenInfixExpression b)
flattenInfixExpression a = [("", a)]

createLeftOrderedInfixExpression (x:xs) = createLeftOrderedInfixExpression' x xs
createLeftOrderedInfixExpression' :: (String, CExpression) -> [(String, CExpression)] -> CExpression
createLeftOrderedInfixExpression' (o, a) ((_, b):[]) = CEInfixOp o a b
createLeftOrderedInfixExpression' (o, a) ((on, b):xs) =
    createLeftOrderedInfixExpression' (on, CEInfixOp o a b) xs

convertVariableDeclaration :: SPCVariableDeclaration -> CompilerStatusT SourcePosition Ifm1VariableDeclaration.VariableDeclaration
convertVariableDeclaration (CVarDec a b) = return $ Ifm1VariableDeclaration.IfVariableDeclaration $ VDec a b

convertStatement :: SPCStatement -> CompilerStatusT SourcePosition Statement
convertStatement (CSExpression a) = do
    n <- convertExpression a
    return $ SExpression n
convertStatement (CSVarDec a) = do
    n <- convertVariableDeclaration a
    return $ SVarDec n


convertParameter :: SPCParameter -> CompilerStatusT SourcePosition Ifm1Parameter.Parameter
convertParameter (CParam a b) = return $ Ifm1Parameter.IfParameter $ PConst a b
convertParameter (CPointerParam a b) = return $ Ifm1Parameter.IfParameter $ PPointer a b
convertParameter (CVariadicParam a b) = return $ Ifm1Parameter.IfParameter $ PVariadic a b

convertFilledParameters :: SPCFilledParameters -> CompilerStatusT SourcePosition [Ifm1Parameter.Parameter]
convertFilledParameters (CMultiParam x xs) = do
    n <- convertParameter x
    m <- convertFilledParameters xs
    return $ n:m
convertFilledParameters (CFinalParam a) = sequence $ [convertParameter a]

convertParameters :: SPCParameters -> CompilerStatusT SourcePosition Ifm1Parameter.Parameters
convertParameters CPEmpty = return $ StandardBlock []
convertParameters (CPParams a) = do
    n <- convertFilledParameters a
    return $ StandardBlock n

convertFilledBlock :: SPCFilledBlock -> CompilerStatusT SourcePosition [SourcePosition Statement]
convertFilledBlock spcFilledBlock = case getContents spcFilledBlock of
    CMultiStatement x xs -> let positionedN = convertStatement x in
        let positionedM = convertFilledBlock xs in do
        n <- positionedN
        m <- positionedM
        lift $ changeContents spcFilledBlock $ (changeContents positionedN n):m
    CFinalStatement x -> sequence [convertStatement x]

convertBlockStatement :: SPCBlockStatement -> CompilerStatusT SourcePosition BlockStatement
convertBlockStatement spcBlockStatement = case getContents spcBlockStatement of
    CBlockEmpty -> lift $ changeContents spcBlockStatement $ SequentialBlock []
    CFilledBlock a -> let positionedN = convertFilledBlock a in do
        n <- positionedN
        lift $ changeContents spcBlockStatement $ SequentialBlock n

convertFunctionDefinition :: SPCFunctionDefinition -> CompilerStatusT SourcePosition FunctionDefinition
convertFunctionDefinition spcFunctionDefinition = case getContents spcFunctionDefinition of
    CFuncDefNative b a c -> let positionedN = convertParameters c in do
        n <- positionedN
        lift $ changeContents spcFunctionDefinition $ FDNative a b $ changeContents (runCompilerStatusT positionedN) n
    CFuncDef b a c d -> let positionedN = convertParameters c in
        let positionedM = convertBlockStatement d in do
        n <- positionedN
        m <- positionedM
        lift $ changeContents spcFunctionDefinition $
            FDNoSyn a b (changeContents (runCompilerStatusT positionedN) n) (changeContents (runCompilerStatusT positionedM) m)
    COpOverloadDef a b c d e -> let positionedN = convertParameters d in
        let positionedM = convertBlockStatement e in do
        n <- positionedN
        m <- positionedM
        lift $ changeContents spcFunctionDefinition $
            FDOperatorOverload b c a (changeContents (runCompilerStatusT positionedN) n) (changeContents (runCompilerStatusT positionedM) m)
    CBracketOpOverloadDef a b c d -> let positionedN = convertParameters c in
        let positionedM = convertBlockStatement d in do
        n <- positionedN
        m <- positionedM
        lift $ changeContents spcFunctionDefinition $
            FDBracketOverload b a (changeContents (runCompilerStatusT positionedN) n) (changeContents (runCompilerStatusT positionedM) m)

convertAliasDefinition :: SPCAliasDefinition -> CompilerStatusT SourcePosition AliasDefinition
convertAliasDefinition spcAliasDefinition = case getContents spcAliasDefinition of
    CAliasDef a b -> lift $ changeContents spcAliasDefinition $ ADNoSyn a b
    CNativeAliasDef a b -> lift $ changeContents spcAliasDefinition $ ADNative a b

convertProgramStatement :: SPCProgramStatement -> CompilerStatusT SourcePosition ProgramStmt
convertProgramStatement spcProgramStatement = case getContents spcProgramStatement of
    CPSVarDec a -> let positionedN = convertVariableDeclaration a in do
        n <- positionedN
        lift $ changeContents spcProgramStatement $ PSVarDec $ changeContents (runCompilerStatusT positionedN) n
    CPSFuncDef a -> let positionedN = convertFunctionDefinition a in do
        n <- positionedN
        lift $ changeContents spcProgramStatement $ PSFuncDef $ changeContents (runCompilerStatusT positionedN) n
    CPSAliasDef a -> let positionedN = convertAliasDefinition a in do
        n <- positionedN
        lift $ changeContents spcProgramStatement $ PSAliasDef $ changeContents (runCompilerStatusT positionedN) n
    CPSImportStatement a ->
        CompilerStatusT $ changeContents spcProgramStatement $ Error "COMPILER ERROR: Import statements should not be present in this context" (show $ CPSImportStatement a)
convertProgram :: SPCProgram -> CompilerStatusT SourcePosition PreProgram
convertProgram program = do
    convertedImportStatements <- convertImportStatements importStatements
    convertedProgramStatements <- convertProgramStatements programStatements
    return $ PreProgram convertedImportStatements convertedProgramStatements
    where
        flattenProgramStatements x = case getContents x of
            CProgramEnd -> []
            CProgram x xs -> x:(flattenProgramStatements xs)
        -- flattenProgramStatements CProgramEnd = []
        -- flattenProgramStatements (CProgram x xs) = x:(flattenProgramStatements xs)
        flattenedStatements = flattenProgramStatements program
        importStatements = filter importStatementPredicate flattenedStatements
        programStatements = filter (not.importStatementPredicate) flattenedStatements
        importStatementPredicate x = case getContents x of
            (CPSImportStatement _) -> True
            _ -> False

convertImportStatements :: [SPCProgramStatement] -> CompilerStatusT SourcePosition Ifm1ImportStatement.ImportStatements
convertImportStatements importStatements = do
    ifm1ImportStatements <- sequence $ map convertImportStatement importStatements
    return $ StandardBlock ifm1ImportStatements

convertProgramStatements :: [SPCProgramStatement] -> CompilerStatusT SourcePosition Program
convertProgramStatements programStatements = do
    ifm1ProgramStatements <- sequence $ map convertProgramStatement programStatements
    lift $ changeContents (sequence programStatements) $ StandardBlock ifm1ProgramStatements

convertImportStatement :: SPCProgramStatement -> CompilerStatusT SourcePosition Ifm1ImportStatement.ImportStatement
convertImportStatement spcProgramStatement = case getContents spcProgramStatement of
    CPSImportStatement a -> convertImportStatement' a
    otherwise -> CompilerStatusT $ changeContents spcProgramStatement $ Error "COMPILER ERROR: Only import statements should be present in this context" (show spcProgramStatement)

convertImportStatement' spcImportStatement = case getContents spcImportStatement of
    CNSImport a -> do
        n <- convertModuleName a
        lift $ changeContents spcImportStatement $ 
            Ifm1ImportStatement.IfImportStatement $ changeContents spcImportStatement $ NSImport n
    CNativeImport a -> do
        n <- convertModuleName a
        lift $ changeContents spcImportStatement $ 
            Ifm1ImportStatement.IfImportStatement $ changeContents spcImportStatement $ NativeImport n

convertModuleName :: SPCModuleName -> CompilerStatusT SourcePosition [SourcePosition String]
convertModuleName spModuleName = case getContents spModuleName of
    CModuleIdent moduleName -> lift $ changeContents spModuleName [changeContents spModuleName moduleName]
    CPackage parentPackage childPackage -> 
        let positionedRestModuleNames = runCompilerStatusT $ convertModuleName childPackage in do
        rest <- CompilerStatusT positionedRestModuleNames
        lift $ changeContents spModuleName $ (headContents spModuleName positionedRestModuleNames parentPackage):rest

