module Com.NoSyn.Evaluation.Program.Program (programEnvironmentEvaluate, programEnvironmentEvaluateIfElement) where

import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Environment.ProgramEnvironment
import Com.NoSyn.Evaluation.Program.Internal.AliasEvaluation
import Com.NoSyn.Evaluation.Program.Internal.FunctionEvaluation
import Com.NoSyn.Evaluation.Program.Internal.VariableDeclarationEvaluation
import Com.NoSyn.Ast.If.Program
import Com.NoSyn.Ast.If.PreProgram
import Com.NoSyn.Ast.If.IfElement
import Data.Map

programEnvironmentEvaluate::ProgramEnvironment -> Program -> CompilerStatus ProgramEnvironment
programEnvironmentEvaluate initialEnvironment@(initialAliasEnvironment, initialFunctionEnvironment, initialVariableEnvironment) program = do
    aliasEnvironment <- programAliasEvaluate initialAliasEnvironment program
    currentProgramFunctionEnvironment <- programFunctionDefinitionEvaluate aliasEnvironment program
    currentProgramVariableEnvironment <- programVariableDeclarationEvaluate aliasEnvironment program
    let functionEnvironment = union currentProgramFunctionEnvironment initialFunctionEnvironment in
        let variableEnvironment = union currentProgramVariableEnvironment initialVariableEnvironment in
            return (aliasEnvironment, functionEnvironment, variableEnvironment)

programEnvironmentEvaluateIfElement :: IfElement -> CompilerStatus ProgramEnvironment
programEnvironmentEvaluateIfElement (IfPreProgram (PreProgram _ program)) =
    programEnvironmentEvaluate defaultProgramEnvironment program
programEnvironmentEvaluateIfElement (IfProgram program) =
    programEnvironmentEvaluate defaultProgramEnvironment program
