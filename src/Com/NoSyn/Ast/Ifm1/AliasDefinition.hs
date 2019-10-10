module Com.NoSyn.Ast.Ifm1.AliasDefinition where

import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Data.Types
import qualified Com.NoSyn.Ast.If.AliasDefinition as IfAliasDefinition
import qualified Com.NoSyn.Ast.If.IfElement as IfElement

data AliasDefinition =
    ADNative Ident String
    | ADNoSyn Ident Ident
    deriving Show

instance IfElementGeneratable AliasDefinition where
    generateIfElement programEnvironment (ADNative aliasName aliasType) =
        return $ IfElement.IfAliasDefinition $ return $ (IfAliasDefinition.ADNative aliasName aliasType)
    generateIfElement programEnvironment (ADNoSyn aliasName aliasType) =
        return $ IfElement.IfAliasDefinition $ return $ (IfAliasDefinition.ADNoSyn aliasName aliasType)
