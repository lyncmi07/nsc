{-# LANGUAGE TemplateHaskell #-}
module Com.NoSyn.Ast.Templates.IfElement where

import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable

data Empty = Empty
createIfElement :: Name -> String -> Q [Dec]
createIfElement elementName elementNameStr = do
    elementClass <- createIfElementClass elementName className generationFunctionName
    emptyInstance <- createEmptyInstance className generationFunctionName
    return $ elementClass ++ emptyInstance
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
                    (AppT (ConT ''CompilerStatus) (AppT (ConT elementName) (VarT a)))))]]
createEmptyInstance :: Name -> Name -> Q [Dec]
createEmptyInstance className generationFunctionName =
    --[d| instance $(conT className) Nothing where 
    return $ [InstanceD Prelude.Nothing [] (AppT (ConT className) (ConT ''Empty))
             [ FunD generationFunctionName [Clause [WildP, WildP] (NormalB (AppE (ConE 'Error) (LitE (StringL "Invalid transformation")))) []] ]]

ifm1GeneratorExpression :: Name -> Name -> String -> ExpQ
ifm1GeneratorExpression a b elementNameStr = do
    n <- newName "n"
    [| case $(varE b) of
            $(return $ ConP ifBox [VarP n]) -> do
                m <- $(varE generationFunction) $(varE a) $(varE n)
                generateD $(varE a) m |]
    where
        generationFunction = mkName $ "generateIf" ++ elementNameStr
        ifBox = mkName "Ifm1Element"
