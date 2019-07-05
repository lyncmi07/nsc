{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Com.NoSyn.Ast.Ifm1.Parameter where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Parameter as IfParameter
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Traits.Listable
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Error.CompilerStatus

type Parameters = Block Parameter
data Parameter = 
    IfParameter IfParameter.Parameter
    deriving Show

instance IfElementGeneratable Parameter where
    generateIfElement programEnvironment (IfParameter a) =
        return $ IfElement.IfParameter a

instance IfElementGeneratable Parameters where
    generateIfElement programEnvironment blockParameters = do
        ifElements <- sequence $ map (generateIfElement programEnvironment)  parameterList
        ifParameters <- extractIfParameters ifElements
        return $ IfElement.IfParameters (StandardBlock ifParameters)
        where
            parameterList = toList blockParameters 

extractIfParameters :: [IfElement.IfElement] -> CompilerStatus [IfParameter.Parameter]
extractIfParameters [] = return []
extractIfParameters ((IfElement.IfParameter x):xs) = do
    xsm <- extractIfParameters xs
    return $ x:xsm
extractIfParameters a = Error "Unexpected non-parameter" (show a)
