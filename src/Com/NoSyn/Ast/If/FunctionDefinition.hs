module Com.NoSyn.Ast.If.FunctionDefinition where

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
import Com.NoSyn.Environment.ProgramEnvironment

data FunctionDefinition =
    FDNative Ident Ident Parameters    
    | FDNoSyn Ident Ident Parameters BlockStatement
    deriving Show

instance EnvironmentUpdater FunctionDefinition where
    updateEnvironment programEnvironment (FDNoSyn funcName funcReturnType parameters _) =
        updateEnvironment (programEnvironment { scopeReturnType = funcReturnType }) parameters
    updateEnvironment programEnvironment _ = return programEnvironment

instance TargetCodeGeneratable FunctionDefinition where
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
        noSynReturnType <- getNoSynType programEnvironment func
        let parameterNoSynTypes = Prelude.map getAlphaTypeName (Listable.toList parameters) in
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
    getAlphaTypeName = getTypeNoCheck
