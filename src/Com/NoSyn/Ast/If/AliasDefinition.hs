module Com.NoSyn.Ast.If.AliasDefinition where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Data.Types
import Com.NoSyn.Error.CompilerStatus
import Data.Map.Ordered

data AliasDefinition =
    ADNative Ident String
    | ADNoSyn Ident Ident
    deriving Show

instance TargetCodeGeneratable AliasDefinition where
    generateD _ (ADNative _ _) = return "";
    generateD _ (ADNoSyn _ _) = return "";

instance EnvironmentUpdater AliasDefinition where
    updateEnvironment programEnvironment@(PE { aliases = aliasEnvironment }) a@(ADNative _ _) =
        Error "Native aliases have not been implemented yet" (show a)
    updateEnvironment programEnvironment@(PE { aliases = aliasEnvironment }) (ADNoSyn aliasName aliasType) = do
        _ <- lookupDType aliasType aliasEnvironment
        let updatedAliasEnvironment = (aliasName, aliasType) |< aliasEnvironment in
            return (programEnvironment { aliases = updatedAliasEnvironment })
