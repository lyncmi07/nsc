module Com.NoSyn.Ast.Ifm1.FunctionDefinition where

import Prelude hiding (getContents )
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
    generateIfElement programEnvironment (FDNative a b parameters) = do
        ~(IfElement.IfParameters ifParameters) <- generateIfElement programEnvironment (getContents parameters)
        return $ IfElement.IfFunctionDefinition (IfFunctionDefinition.FDNative a b ifParameters)
    generateIfElement programEnvironment (FDNoSyn funcName returnType parameters blockStatement) = do
        ~(IfElement.IfParameters ifParameters) <- generateIfElement programEnvironment (getContents parameters)
        ~(IfElement.IfBlockStatement ifBlockStatement) <- generateIfElement programEnvironment (getContents blockStatement)
        return $ IfElement.IfFunctionDefinition (IfFunctionDefinition.FDNoSyn (funcName++"_function") returnType ifParameters ifBlockStatement)
    generateIfElement programEnvironment (FDOperatorOverload operatorType operatorString returnType parameters blockStatement) = do
        ~(IfElement.IfParameters ifParameters) <- generateIfElement programEnvironment (getContents parameters)
        ~(IfElement.IfBlockStatement ifBlockStatement) <- generateIfElement programEnvironment (getContents blockStatement)
        namedOperators <- operatorStringConverter operatorString
        return $ IfElement.IfFunctionDefinition (IfFunctionDefinition.FDNoSyn ((show operatorType)++"_"++(concat namedOperators)++"_operator") returnType ifParameters ifBlockStatement)
    generateIfElement programEnvironment (FDBracketOverload bracketType returnType parameters blockStatement) = do
        ~(IfElement.IfParameters ifParameters) <- generateIfElement programEnvironment (getContents parameters)
        ~(IfElement.IfBlockStatement ifBlockStatement) <- generateIfElement programEnvironment (getContents blockStatement)
        return $ IfElement.IfFunctionDefinition (IfFunctionDefinition.FDNoSyn ((show bracketType)++"_bracketop") returnType ifParameters ifBlockStatement)
