module Com.NoSyn.Ast.Ifm1.FunctionDefinition where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.FunctionDefinition as IfFunctionDefinition
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Ast.Ifm1.Parameter
import Com.NoSyn.Ast.Ifm1.Statement
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Operators


data FunctionDefinition =
    FDNative Ident Ident Parameters
    | FDNoSyn Ident Ident Parameters BlockStatement
    | FDOperatorOverload OperatorType String Ident Parameters BlockStatement

instance IfElementGeneratable FunctionDefinition where
    generateIfElement programEnvironment (FDNative a b parameters) = do
        ~(IfElement.IfParameters ifParameters) <- generateIfElement programEnvironment parameters
        return $ IfElement.IfFunctionDefinition (IfFunctionDefinition.FDNative a b ifParameters)
    generateIfElement programEnvironment (FDNoSyn funcName returnType parameters blockStatement) = do
        ~(IfElement.IfParameters ifParameters) <- generateIfElement programEnvironment parameters
        ~(IfElement.IfBlockStatement ifBlockStatement) <- generateIfElement programEnvironment blockStatement
        return $ IfElement.IfFunctionDefinition (IfFunctionDefinition.FDNoSyn (funcName++"_function") returnType ifParameters ifBlockStatement)
    generateIfElement programEnvironment (FDOperatorOverload operatorType operatorString returnType parameters blockStatement) = do
        ~(IfElement.IfParameters ifParameters) <- generateIfElement programEnvironment parameters
        ~(IfElement.IfBlockStatement ifBlockStatement) <- generateIfElement programEnvironment blockStatement
        namedOperators <- operatorStringConverter operatorString
        return $ IfElement.IfFunctionDefinition (IfFunctionDefinition.FDNoSyn ((show operatorType)++"_"++(concat namedOperators)++"_operator") returnType ifParameters ifBlockStatement)
