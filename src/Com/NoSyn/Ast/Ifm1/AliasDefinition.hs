module Com.NoSyn.Ast.Ifm1.AliasDefinition where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Data.Types
import qualified Com.NoSyn.Ast.If.AliasDefinition as IfAliasDefinition
import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import Com.NoSyn.Error.SourcePosition

data AliasDefinition =
    ADNative Ident String
    | ADNoSyn Ident Ident
    deriving Show

instance IfElementGeneratable AliasDefinition where
    generateIfElement programEnvironment spAliasDef = case getContents spAliasDef of
        ADNative aliasName aliasType ->
            return $ changeContents spAliasDef $ IfElement.IfAliasDefinition $ return $ (IfAliasDefinition.ADNative aliasName aliasType)
        ADNoSyn aliasName aliasType ->
            return $ changeContents spAliasDef $ IfElement.IfAliasDefinition $ return $ (IfAliasDefinition.ADNoSyn aliasName aliasType)
