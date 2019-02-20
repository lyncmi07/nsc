module Com.NoSyn.Evaluation.Program.Internal.VariableDeclarationEvaluation (programVariableDeclarationEvaluate) where

import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Environment.AliasEnvironment
import Com.NoSyn.Environment.VariableEnvironment
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Ast.VariableDeclaration
import Com.NoSyn.Ast.Listable
import Com.NoSyn.Ast.Program
import Data.Map

programVariableDeclarationEvaluate::AliasEnvironment -> Program -> CompilerStatus VariableEnvironment
programVariableDeclarationEvaluate aliasEnvironment program = do
    variableDeclarations <- programVariableDeclarationEvaluate' aliasEnvironment (Com.NoSyn.Ast.Listable.toList program)    
    return $ Data.Map.fromList variableDeclarations

programVariableDeclarationEvaluate'::AliasEnvironment -> [ProgramStmt] -> CompilerStatus [(Ident, Variable)]
programVariableDeclarationEvaluate' _ [] = return []
programVariableDeclarationEvaluate' aliasEnvironment ((PSVarDec variableDeclaration):xs) = do
    varDecEntry <- createVariableDeclarationEntry aliasEnvironment variableDeclaration
    otherVarDecs <- programVariableDeclarationEvaluate' aliasEnvironment xs
    return $ varDecEntry:otherVarDecs
programVariableDeclarationEvaluate' aliasEnvironment (_:xs) = do
    programVariableDeclarationEvaluate' aliasEnvironment xs
    
createVariableDeclarationEntry::AliasEnvironment -> VariableDeclaration -> CompilerStatus (Ident, Variable)
createVariableDeclarationEntry aliasEnvironment (VDec varType varName) = do
    _ <- lookupDType varType aliasEnvironment
    return $ (varName, (VConst varType varName))
