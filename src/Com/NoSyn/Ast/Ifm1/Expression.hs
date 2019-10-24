module Com.NoSyn.Ast.Ifm1.Expression where

import Prelude hiding (getContents)
import qualified Com.NoSyn.Ast.If.IfElement as IfElement
import qualified Com.NoSyn.Ast.If.Expression as IfExpression
import Com.NoSyn.Ast.Traits.IfElementGeneratable
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Operators
import Com.NoSyn.Ast.Ifm1.Constant
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.SourcePositionIfGenerator
import qualified Data.Map as Map

data Expression =
    EFuncCall Ident [SourcePosition Expression]
    | EConst (SourcePosition Constant)
    | EIdent Ident
    | EOp OperatorType String [SourcePosition Expression]
    | EBrackets BracketType [SourcePosition Expression]
    deriving Show

instance IfElementGeneratable Expression where
    generateIfElement programEnvironment spExpression = do
        ifExpression <- generateIfExpression programEnvironment spExpression
        return $ changeContents spExpression $ IfElement.IfExpression ifExpression

generateIfExpression :: ProgramEnvironment -> SourcePosition Expression -> CompilerStatus (SourcePosition IfExpression.Expression)
generateIfExpression programEnvironment@(PE { functions = functions }) expression = case getContents expression of
    EFuncCall funcName parameters -> do
        ifParameters <- generateIfParameters parameters
        return $ changeContents expression $ IfExpression.EFuncCall (funcName ++ "_function") ifParameters
    EConst constant -> do
        positionedIfConstant <- generateIfElement programEnvironment constant
        let ~(IfElement.IfConstant ifConstant) = getContents positionedIfConstant
        return $ changeContents expression $ IfExpression.EConst ifConstant
    EIdent ident -> do
        return $ changeContents expression $ IfExpression.EIdent ident
    EOp operatorType operatorString parameters -> do
        namedOperators <- operatorStringConverter operatorString
        ifParameters <- generateIfParameters parameters
        let ifFunctionName = (show operatorType) ++ "_" ++ (concat namedOperators) ++ "_operator" in
            return $ changeContents expression $ IfExpression.EFuncCall ifFunctionName ifParameters
    EBrackets bracketType parameters@(x:xs) ->
        case getContents x of
            (EIdent a) 
                | a `Map.member` functions -> generateIfExpression programEnvironment (changeContents x $ EFuncCall a xs)
                | (a ++ "_function") `Map.member` functions -> generateIfExpression programEnvironment (changeContents x $ EFuncCall a xs)
            _ -> do 
                    ifParameters <- generateIfParameters parameters
                    let ifFunctionName = (show bracketType) ++ "_bracketop" in
                        return $ changeContents expression $ IfExpression.EFuncCall ifFunctionName ifParameters
    where
        generateIfParameters parameters = sequence $ map (generateIfExpression programEnvironment) parameters
