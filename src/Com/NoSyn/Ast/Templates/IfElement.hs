{-# LANGUAGE TemplateHaskell #-}
module Com.NoSyn.Ast.Templates.IfElement where

import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable

createIfElement :: Name -> String -> Q [Dec]
createIfElement elementName elementNameStr = do
    elementClass <- createIfElementClass elementName className generationFunctionName
    targetGenInstance <- createTargetCodeGeneratableInstance className generationFunctionName
    return $ elementClass ++ targetGenInstance
    where
        className = mkName ("If" ++ elementNameStr)
        generationFunctionName = mkName $ "generateIf" ++ elementNameStr

createIfElementClass :: Name -> Name -> Name -> Q [Dec]
createIfElementClass elementName className generationFunctionName = do
    a <- newName "a"
    return $ [ClassD [] className [PlainTV a] [] 
        [SigD generationFunctionName 
            (AppT (AppT ArrowT (ConT ''ProgramEnvironment))
                (AppT (AppT ArrowT (VarT a))
                    (AppT (ConT ''CompilerStatus) (ConT elementName))))]]

createTargetCodeGeneratableInstance :: Name -> Name -> Q [Dec]
createTargetCodeGeneratableInstance className generationFunctionName =
    [d|instance $(conT className) a => TargetCodeGeneratable a where
            generateD programEnvironment x = do
                y <- $(varE generationFunctionName) programEnvironment x
                generateD programEnvironment y|]
