module Com.NoSyn.Ast.If.Parameter where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.Typeable
import Com.NoSyn.Ast.Helpers.TypeCheckFunctions
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Ast.Traits.Listable as Listable
import Data.List
import Data.Map
import Com.NoSyn.Environment.ProgramEnvironment

type Parameters = Block Parameter


data Parameter = 
    PConst Ident Ident
    | PPointer Ident Ident
    deriving Show

instance EnvironmentUpdater Parameter where
    updateEnvironment programEnvironment@(PG {variables = variableEnvironment}) param@(PConst paramType paramName) = do
        verifiedType <- getNoSynType programEnvironment param
        return (programEnvironment { variables = Data.Map.insert paramName (VConst verifiedType paramName) variableEnvironment})
    updateEnvironment programEnvironment@(PG { variables = variableEnvironment}) param@(PPointer paramType paramName) = do
        verifiedType <- getNoSynType programEnvironment param
        return (programEnvironment { variables = Data.Map.insert paramName (VPointer verifiedType paramName) variableEnvironment})

instance TargetCodeGeneratable Parameter where
    generateD programEnvironment parameter@(PConst paramType paramName) = do
        parameterDType <- getRealType programEnvironment parameter
        return $ parameterDType ++ " " ++ paramName
    generateD programEnvironment parameter@(PPointer paramType paramName) = do
        parameterDType <- getRealType programEnvironment parameter
        return $ parameterDType ++ "* " ++ paramName

instance Typeable Parameter where
    getTypeNoCheck (PConst paramType _) = paramType
    getTypeNoCheck (PPointer paramType _) = paramType

instance Blockable Parameter where
    blockSeparator _ = ", "

parameterToTuple::Parameter -> (Ident, Variable)
parameterToTuple (PConst paramType paramName) = 
    (paramName , (VConst paramType paramName))
parameterToTuple (PPointer paramType paramName) = 
    (paramName , (VPointer paramType paramName))

parametersToTuples::Parameters -> [(Ident, Variable)]
parametersToTuples parameters =
    Prelude.map parameterToTuple (Listable.toList parameters)
