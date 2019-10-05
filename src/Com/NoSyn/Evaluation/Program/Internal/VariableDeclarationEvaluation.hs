module Com.NoSyn.Evaluation.Program.Internal.VariableDeclarationEvaluation (programVariableDeclarationEvaluate) where

import Prelude hiding (getContents)
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Environment.AliasEnvironment
import Com.NoSyn.Environment.VariableEnvironment
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Ast.If.VariableDeclaration
import Com.NoSyn.Ast.Traits.Listable as Listable
import Com.NoSyn.Ast.If.Program
import Data.Map

programVariableDeclarationEvaluate::AliasEnvironment -> Program -> CompilerStatus VariableEnvironment
programVariableDeclarationEvaluate aliasEnvironment program = do
    variableDeclarations <- programVariableDeclarationEvaluate' aliasEnvironment (Listable.toList program)    
    return $ Data.Map.fromList variableDeclarations

programVariableDeclarationEvaluate'::AliasEnvironment -> [ProgramStmt] -> CompilerStatus [(Ident, Variable)]
programVariableDeclarationEvaluate' _ [] = return []
programVariableDeclarationEvaluate' aliasEnvironment ((PSVarDec variableDeclaration):xs) = do
    varDecEntry <- createVariableDeclarationEntry aliasEnvironment (getContents variableDeclaration)
    otherVarDecs <- programVariableDeclarationEvaluate' aliasEnvironment xs
    return $ varDecEntry:otherVarDecs
programVariableDeclarationEvaluate' aliasEnvironment (_:xs) = do
    programVariableDeclarationEvaluate' aliasEnvironment xs
    
createVariableDeclarationEntry::AliasEnvironment -> VariableDeclaration -> CompilerStatus (Ident, Variable)
createVariableDeclarationEntry aliasEnvironment (VDec varType varName) = do
    _ <- lookupDType varType aliasEnvironment
    return $ (varName, (VConst varType varName))
