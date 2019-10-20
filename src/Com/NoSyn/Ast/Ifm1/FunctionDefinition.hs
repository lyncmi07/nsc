module Com.NoSyn.Ast.Ifm1.FunctionDefinition where

import Prelude hiding (getContents)
import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.FunctionDefinition as IfFunctionDefinition
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Ifm1.Parameter
import Com.NoSyn.Ast.Ifm1.Statement
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Operators
import Com.NoSyn.Error.SourcePosition


data FunctionDefinition =
    FDNative Ident Ident (SourcePosition Parameters)
    | FDNoSyn Ident Ident (SourcePosition Parameters) (SourcePosition BlockStatement)
    | FDOperatorOverload OperatorType String Ident (SourcePosition Parameters) (SourcePosition BlockStatement)
    | FDBracketOverload BracketType Ident (SourcePosition Parameters) (SourcePosition BlockStatement)
    deriving Show

instance IfElementGeneratable FunctionDefinition where
    generateIfElement programEnvironment spFunctionDef = case getContents spFunctionDef of
        FDNative a b parameters -> do
            positionedParameters <- generateIfElement programEnvironment parameters
            ~(IfElement.IfParameters ifParameters) <- return $ getContents positionedParameters
            return $ changeContents spFunctionDef $
                IfElement.IfFunctionDefinition $
                changeContents spFunctionDef $ (IfFunctionDefinition.FDNative a b ifParameters)
        FDNoSyn funcName returnType parameters blockStatement -> do
            positionedParameters <- generateIfElement programEnvironment parameters
            positionedBlockStatement <- generateIfElement programEnvironment blockStatement
            ~(IfElement.IfParameters ifParameters) <- return $ getContents positionedParameters
            ~(IfElement.IfBlockStatement ifBlockStatement) <- return $ getContents positionedBlockStatement
            return $ changeContents spFunctionDef $
                IfElement.IfFunctionDefinition $
                changeContents spFunctionDef $ (IfFunctionDefinition.FDNoSyn (funcName++"_function") returnType ifParameters ifBlockStatement)
        FDOperatorOverload operatorType operatorString returnType parameters blockStatement -> do
            positionedParameters <- generateIfElement programEnvironment parameters
            positionedBlockStatement <- generateIfElement programEnvironment blockStatement
            ~(IfElement.IfParameters ifParameters) <- return $ getContents positionedParameters
            ~(IfElement.IfBlockStatement ifBlockStatement) <- return $ getContents positionedBlockStatement
            namedOperators <- operatorStringConverter operatorString
            return $ changeContents spFunctionDef $
                IfElement.IfFunctionDefinition $ 
                changeContents spFunctionDef $ (IfFunctionDefinition.FDNoSyn ((show operatorType)++"_"++(concat namedOperators)++"_operator") returnType ifParameters ifBlockStatement)
        FDBracketOverload bracketType returnType parameters blockStatement -> do
            positionedParameters <- generateIfElement programEnvironment parameters
            positionedBlockStatement <- generateIfElement programEnvironment blockStatement
            ~(IfElement.IfParameters ifParameters) <- return $ getContents positionedParameters
            ~(IfElement.IfBlockStatement ifBlockStatement) <- return $ getContents positionedBlockStatement
            return $ changeContents spFunctionDef $
                IfElement.IfFunctionDefinition $
                changeContents spFunctionDef $ (IfFunctionDefinition.FDNoSyn ((show bracketType)++"_bracketop") returnType ifParameters ifBlockStatement)
