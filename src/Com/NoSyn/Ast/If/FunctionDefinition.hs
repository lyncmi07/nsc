module Com.NoSyn.Ast.If.FunctionDefinition where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.DIdentifiable
import Com.NoSyn.Ast.Traits.Nameable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Helpers.TypeCheckFunctions
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Traits.Listable as Listable
import Com.NoSyn.Data.Types
import Com.NoSyn.Ast.If.Statement
import Com.NoSyn.Ast.If.Parameter
import Data.Map.Ordered
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.SourcePositionTraits
import Com.NoSyn.Environment.ProgramEnvironment

data FunctionDefinition =
    FDNative Ident Ident (SourcePosition Parameters)
    | FDNoSyn Ident Ident (SourcePosition Parameters) (SourcePosition BlockStatement)
    deriving Show

instance EnvironmentUpdater FunctionDefinition where
    updateEnvironment programEnvironment spFunctionDef = case getContents spFunctionDef of
        FDNoSyn funcName funcReturnType parameters _ ->
            updateEnvironment (programEnvironment { scopeReturnType = funcReturnType }) parameters
        otherwise -> return programEnvironment

instance TargetCodeGeneratable FunctionDefinition where
    generateD programEnvironment spFunctionDef = case getContents spFunctionDef of
        FDNative _ _ _ -> return ""
        func@(FDNoSyn funcName returnType parameters blockStatement) -> do
            generatedParameters <- generateD programEnvironment parameters
            updatedProgramEnvironment <- updateEnvironment programEnvironment spFunctionDef
            generatedBlockStatement <- generateD updatedProgramEnvironment blockStatement
            dReturnType <- getRealType programEnvironment func
            functionDName <- getDIdentifier programEnvironment func
            return $ 
                dReturnType 
                ++ " " 
                ++ functionDName
                ++ "(" ++ generatedParameters ++ ")" 
                ++ "{\n" ++ generatedBlockStatement ++ "\n}"

instance DIdentifiable FunctionDefinition where
    getDIdentifier _ (FDNative funcName _ _) = return funcName
    getDIdentifier programEnvironment func@(FDNoSyn funcName returnType parameters _) = do
        noSynReturnType <- getNoSynType programEnvironment func
        parameterNoSynTypes <- sequence $ Prelude.map (getAlphaTypeName (aliases programEnvironment)) (sourcePositionToList parameters)
        return $ funcName
            ++ "_"
            ++ noSynReturnType
            ++ concat parameterNoSynTypes

instance Nameable FunctionDefinition where
    getName (FDNative name _ _) = name
    getName (FDNoSyn name _ _ _) = name
    setName name (FDNative _ a b) = FDNative name a b
    setName name (FDNoSyn _ a b c) = FDNoSyn name a b c

instance Typeable FunctionDefinition where
    getTypeNoCheck (FDNative _ returnType _) = returnType
    getTypeNoCheck (FDNoSyn _ returnType _ _) = returnType
    getAlphaTypeName _ x = return $  getTypeNoCheck x
