module Com.NoSyn.Ast.Ifm1.Expression where

import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Expression as IfExpression
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Operators
import Com.NoSyn.Ast.Ifm1.Constant
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus

data Expression =
    EFuncCall Ident [Expression]
    | EConst Constant
    | EIdent Ident
    | EOp OperatorType String [Expression]
    | EBrackets BracketType [Expression]
    deriving Show

instance IfElementGeneratable Expression where
    generateIfElement programEnvironment expression = do
        ifExpression <- generateIfExpression programEnvironment expression
        return $ IfElement.IfExpression ifExpression

generateIfExpression :: ProgramEnvironment -> Expression -> CompilerStatus IfExpression.Expression
generateIfExpression programEnvironment expression = case expression of
    EFuncCall funcName parameters -> do
        ifParameters <- generateIfParameters parameters
        return $ IfExpression.EFuncCall (funcName ++ "_function") ifParameters
    EConst constant -> do
        ~(IfElement.IfConstant ifConstant) <- generateIfElement programEnvironment constant
        return $ IfExpression.EConst ifConstant
    EIdent ident -> do
        return $ IfExpression.EIdent ident
    EOp operatorType operatorString parameters -> do
        namedOperators <- operatorStringConverter operatorString
        ifParameters <- generateIfParameters parameters
        let ifFunctionName = (show operatorType) ++ "_" ++ (concat namedOperators) ++ "_operator" in
            return $ IfExpression.EFuncCall ifFunctionName ifParameters
    EBrackets bracketType parameters -> do
        ifParameters <- generateIfParameters parameters
        let ifFunctionName = (show bracketType) ++ "_" ++ "_operator" in
            let returnVal = IfExpression.EFuncCall ifFunctionName ifParameters in
                case bracketType of
                    Parentheses -> return returnVal
                    Square -> if (length ifParameters) /= 1
                        then Error "There should only be one parameter to a square bracket operator overload" (show parameters)
                        else return returnVal
                    Curly -> if (length ifParameters) /= 1
                        then Error "There should only be one parameter to a curly bracket operator overload" (show parameters)
                        else return returnVal
    where
        generateIfParameters parameters = sequence $ map (generateIfExpression programEnvironment) parameters
