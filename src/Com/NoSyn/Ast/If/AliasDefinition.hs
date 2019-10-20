module Com.NoSyn.Ast.If.AliasDefinition where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Data.Types
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Data.Map.Ordered

data AliasDefinition =
    ADNative Ident String
    | ADNoSyn Ident Ident
    deriving Show

instance TargetCodeGeneratable AliasDefinition where
    generateD _ spAliasDef = case getContents spAliasDef of
        ADNative _ _ -> return ""
        ADNoSyn _ _ -> return ""

instance EnvironmentUpdater AliasDefinition where
    updateEnvironment programEnvironment@(PE { aliases = aliasEnvironment }) spAliasDef = case getContents spAliasDef of
        ADNative _ _ -> PositionedError (getSourcePosition spAliasDef) "Native aliases have not been implemented yet" (show $ getContents spAliasDef)
        ADNoSyn aliasName aliasType -> do
            _ <- lookupDType aliasType aliasEnvironment
            let updatedAliasEnvironment = (aliasName, aliasType) |< aliasEnvironment in
                return (programEnvironment { aliases = updatedAliasEnvironment })
