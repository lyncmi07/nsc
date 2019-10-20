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
convertConstant spcConstant = case getContents spcConstant of
    CCString a -> 
        lift $ changeContents spcConstant $ Ifm1Constant.IfConstant $ changeContents spcConstant $ CString a
    CCInt a ->
        lift $ changeContents spcConstant $ Ifm1Constant.IfConstant $ changeContents spcConstant $ CInt a
    CCDouble a ->
        lift $ changeContents spcConstant $ Ifm1Constant.IfConstant $ changeContents spcConstant $ CDouble a
    CCChar a ->
        lift $ changeContents spcConstant $ Ifm1Constant.IfConstant $ changeContents spcConstant $ CChar a

convertFilledExpressionList :: SPCFilledExpressionList -> CompilerStatusT SourcePosition [SourcePosition Expression]
convertFilledExpressionList spcFilledExpressionList = case getContents spcFilledExpressionList of
    CMultiExpression x xs -> let positionedN = convertExpression x in do
        n <- positionedN
        m <- convertFilledExpressionList xs
        lift $ changeContents spcFilledExpressionList $ (changeContents (runCompilerStatusT positionedN) n):m
    CFinalExpression x -> let positionedN = convertExpression x in do
        n <- positionedN
        lift $ changeContents spcFilledExpressionList $ [changeContents (runCompilerStatusT positionedN) n]

convertExpressionList :: SPCExpressionList -> CompilerStatusT SourcePosition [SourcePosition Expression]
convertExpressionList spcExpressionList = case getContents spcExpressionList of
    CListEmpty -> return []
    CListNonEmpty a -> convertFilledExpressionList a

convertExpression :: SPCExpression -> CompilerStatusT SourcePosition Expression
convertExpression spcExpression = case getContents spcExpression of
    CEConst a -> let positionedN = convertConstant a in do
        n <- positionedN
        lift $ changeContents spcExpression $ EConst (changeContents (runCompilerStatusT positionedN) n)
    CEIdent a -> lift $ changeContents spcExpression  $ EIdent a
    CEBracketed a -> convertExpression a
    CEBracketOp bracketType a b -> 
        let positionedN = convertExpression a in do
        n <- positionedN
        m <- convertExpressionList b
        lift $ changeContents spcExpression $ EBrackets bracketType (changeContents (runCompilerStatusT positionedN) n:m)
    CEPrefixOp a b ->
        case getContents b of
            CEInfixOp _ _ _ -> convertExpression $ reorderPrefixOperator spcExpression
            _ -> let positionedN = convertExpression b in do
                n <- positionedN
                lift $ changeContents spcExpression  $ EOp Prefix a [changeContents (runCompilerStatusT positionedN) n]
    CEPostfixOp a b -> let positionedN = convertExpression b in do
        n <- positionedN
        lift $ changeContents spcExpression $ EOp Postfix a [changeContents (runCompilerStatusT positionedN) n]
    CEInfixOp _ _ _ ->
        let positionedN = convertExpression a in
        let positionedM = convertExpression b in do
        n <- positionedN
        m <- positionedM
        lift $ changeContents spcExpression  $ EOp Infix o ((changeContents (runCompilerStatusT positionedN) n):(changeContents (runCompilerStatusT positionedM) m):[])
        where
            spcInfixOp = reverseOperatorOrder spcExpression
            (CEInfixOp o a b) = getContents spcInfixOp

reorderPrefixOperator :: SPCExpression -> SourcePosition CExpression
reorderPrefixOperator spcExpression = case getContents spcExpression of
    CEPrefixOp a b -> reorderPrefixOperator' a b

reorderPrefixOperator' :: String -> SourcePosition CExpression -> SourcePosition CExpression
reorderPrefixOperator' prefixOp spcExpression = case getContents spcExpression of
    CEInfixOp a b c -> changeContents spcExpression $ CEInfixOp a (reorderPrefixOperator' prefixOp b) c
    otherwise -> changeContents spcExpression $ CEPrefixOp prefixOp spcExpression


reverseOperatorOrder :: SPCExpression -> SourcePosition CExpression
reverseOperatorOrder a =
    let flattenedExpressions = flattenInfixExpression a in
    createLeftOrderedInfixExpression $ getContents flattenedExpressions

flattenInfixExpression :: SPCExpression -> SourcePosition [(String, SourcePosition CExpression)]
flattenInfixExpression spcExpression = case getContents spcExpression of
    CEInfixOp o a b -> changeContents spcExpression $ (o, a) : (getContents $ flattenInfixExpression b)
    otherwise -> changeContents spcExpression $ ("", spcExpression):[]

createLeftOrderedInfixExpression (x:xs) = createLeftOrderedInfixExpression' x xs
createLeftOrderedInfixExpression' :: (String, SourcePosition CExpression) -> [(String, SourcePosition CExpression)] -> SourcePosition CExpression
createLeftOrderedInfixExpression' (o, a) ((_, b):[]) = changeContents a $ CEInfixOp o a b
createLeftOrderedInfixExpression' (o, a) ((on, b):xs) =
    createLeftOrderedInfixExpression' (on, changeContents a $ CEInfixOp o a b) xs

convertVariableDeclaration :: SPCVariableDeclaration -> CompilerStatusT SourcePosition Ifm1VariableDeclaration.VariableDeclaration
convertVariableDeclaration spcVariableDeclaration = case getContents spcVariableDeclaration of
    CVarDec a b -> 
        lift $ changeContents spcVariableDeclaration $ Ifm1VariableDeclaration.IfVariableDeclaration $ changeContents spcVariableDeclaration $ VDec a b

convertStatement :: SPCStatement -> CompilerStatusT SourcePosition Statement
convertStatement spcStatement = case getContents spcStatement of
    CSExpression a -> let positionedN = convertExpression a in do
        n <- positionedN
        lift $ changeContents spcStatement $ SExpression $ changeContents (runCompilerStatusT positionedN) n
    CSVarDec a -> let positionedN = convertVariableDeclaration a in do
        n <- positionedN
        lift $ changeContents spcStatement $ SVarDec $ changeContents (runCompilerStatusT positionedN) n

convertParameter :: SPCParameter -> CompilerStatusT SourcePosition Ifm1Parameter.Parameter
convertParameter spcParameter = case getContents spcParameter of
    CParam a b -> lift $ changeContents spcParameter $ Ifm1Parameter.IfParameter $ changeContents spcParameter $ PConst a b
    CPointerParam a b -> lift $ changeContents spcParameter $ Ifm1Parameter.IfParameter $ changeContents spcParameter $ PPointer a b
    CVariadicParam a b -> lift $ changeContents spcParameter $ Ifm1Parameter.IfParameter $ changeContents spcParameter $ PVariadic a b

convertFilledParameters :: SPCFilledParameters -> CompilerStatusT SourcePosition [SourcePosition Ifm1Parameter.Parameter]
convertFilledParameters spcFilledParameters = case getContents spcFilledParameters of
    CMultiParam x xs -> 
        let positionedN = convertParameter x in do
        n <- positionedN
        m <- convertFilledParameters xs
        lift $ changeContents spcFilledParameters $ (changeContents (runCompilerStatusT positionedN) n):m
    CFinalParam a -> let positionedN = convertParameter a in do
        n <- positionedN
        lift $ changeContents a $ [changeContents (runCompilerStatusT positionedN) n]

convertParameters :: SPCParameters -> CompilerStatusT SourcePosition Ifm1Parameter.Parameters
convertParameters spcParameters = case getContents spcParameters of
    CPEmpty -> lift $ changeContents spcParameters $ StandardBlock []
    CPParams a -> let positionedN = convertFilledParameters a in do
        n <- positionedN
        lift $ changeContents spcParameters $ StandardBlock n

convertFilledBlock :: SPCFilledBlock -> CompilerStatusT SourcePosition [SourcePosition Statement]
convertFilledBlock spcFilledBlock = case getContents spcFilledBlock of
    CMultiStatement x xs -> let positionedN = convertStatement x in
        let positionedM = convertFilledBlock xs in do
        n <- positionedN
        m <- positionedM
        lift $ changeContents spcFilledBlock $ (changeContents (runCompilerStatusT positionedN) n):m
    CFinalStatement x -> let positionedStatement = convertStatement x in do
        statement <- positionedStatement
        lift $ changeContents x $ [changeContents (runCompilerStatusT positionedStatement) statement]

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
convertProgram spcProgram = 
    let csN = convertImportStatements importStatements in
    let csM = convertProgramStatements programStatements in do
    n <- csN
    m <- csM
    lift $ changeContents spcProgram $ PreProgram (changeContents (runCompilerStatusT csN) n) (changeContents (runCompilerStatusT csM) m)
    where
        flattenProgramStatements x = case getContents x of
            CProgramEnd -> []
            CProgram x xs -> x:(flattenProgramStatements xs)
            CProgramFunction x xs -> x:(flattenProgramStatements xs)
        -- flattenProgramStatements CProgramEnd = []
        -- flattenProgramStatements (CProgram x xs) = x:(flattenProgramStatements xs)
        flattenedStatements = flattenProgramStatements spcProgram
        importStatements = filter importStatementPredicate flattenedStatements
        programStatements = filter (not.importStatementPredicate) flattenedStatements
        importStatementPredicate x = case getContents x of
            (CPSImportStatement _) -> True
            _ -> False

convertImportStatements :: [SPCProgramStatement] -> CompilerStatusT SourcePosition Ifm1ImportStatement.ImportStatements
convertImportStatements importStatements = let positionedStatements = map convertImportStatement importStatements in do
    ifm1ImportStatements <- sequence positionedStatements
    lift $ changeContents (sequence importStatements) $ StandardBlock (map (uncurry changeContents) $ zip (map runCompilerStatusT positionedStatements) ifm1ImportStatements)

convertProgramStatements :: [SPCProgramStatement] -> CompilerStatusT SourcePosition Program
convertProgramStatements programStatements = let positionedStatements = map convertProgramStatement programStatements in do
    ifm1ProgramStatements <- sequence positionedStatements
    lift $ changeContents (sequence programStatements) $ StandardBlock (map (uncurry changeContents) $ zip (map runCompilerStatusT positionedStatements) ifm1ProgramStatements)

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

