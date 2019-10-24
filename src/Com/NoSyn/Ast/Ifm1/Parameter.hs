{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Com.NoSyn.Ast.Ifm1.Parameter where

import Prelude hiding (getContents)
import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Parameter as IfParameter
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Traits.Listable
import Com.NoSyn.Ast.If.Block
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.SourcePositionIfGenerator

type Parameters = Block Parameter
data Parameter = 
    IfParameter (SourcePosition IfParameter.Parameter)
    deriving Show

instance IfElementGeneratable Parameter where
    generateIfElement programEnvironment spIfParameter = case getContents spIfParameter of
        IfParameter a ->
            return $ changeContents spIfParameter $ IfElement.IfParameter a
        
instance IfElementGeneratable Parameters where
    generateIfElement programEnvironment spBlockParameters = do
        ifElements <- sequence $ map (generateIfElement programEnvironment)  parameterList
        ifParameters <- extractIfParameters ifElements
        return $ changeContents spBlockParameters $ IfElement.IfParameters (changeContents (sequence ifParameters) (StandardBlock ifParameters))
        where
            parameterList = toSourcePositionedList $ getContents spBlockParameters

extractIfParameters :: [SourcePosition IfElement.IfElement] -> CompilerStatus [SourcePosition IfParameter.Parameter]
extractIfParameters [] = return []
extractIfParameters (spIfParameter:xs) = case getContents spIfParameter of
    IfElement.IfParameter x -> do
        xsm <- extractIfParameters xs
        return $ x:xsm
    otherwise -> PositionedError (getSourcePosition spIfParameter) "Unexpected non-parameter" (show $ getContents spIfParameter)
