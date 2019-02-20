module Com.NoSyn.Ast.FunctionDefinition where

import Com.NoSyn.Ast.AstElement
import Com.NoSyn.Ast.DIdentifiable
import Com.NoSyn.Ast.EnvironmentUpdater
import Com.NoSyn.Data.Types
import Com.NoSyn.Ast.Statement
import Com.NoSyn.Ast.Parameter
import Com.NoSyn.Ast.Typeable
import Com.NoSyn.Ast.TypeCheckFunctions
import Com.NoSyn.Ast.Listable
import Data.Map.Ordered
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Environment.ProgramEnvironment

data FunctionDefinition =
    FDNative Ident Ident Parameters    
    | FDNoSyn Ident Ident Parameters BlockStatement

instance EnvironmentUpdater FunctionDefinition where
    updateEnvironment programEnvironment (FDNoSyn funcName _ parameters _) =
        updateEnvironment programEnvironment parameters
    updateEnvironment programEnvironment _ = return programEnvironment

instance AstElement FunctionDefinition where
    generateD _ (FDNative _ _ _) = return ""
    generateD programEnvironment func@(FDNoSyn funcName returnType parameters blockStatement) = do
        generatedParameters <- generateD programEnvironment parameters
        updatedProgramEnvironment <- updateEnvironment programEnvironment func
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
        _ <- getNoSynType programEnvironment func
        parameterNoSynTypes <- sequence $ Prelude.map (getNoSynType programEnvironment) (Com.NoSyn.Ast.Listable.toList parameters)
        return $ funcName 
            ++ "_" 
            ++ returnType
            ++ concat parameterNoSynTypes

instance Typeable FunctionDefinition where
    getTypeNoCheck (FDNative _ returnType _) = returnType
    getTypeNoCheck (FDNoSyn _ returnType _ _) = returnType
