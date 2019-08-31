{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Com.NoSyn.Environment.ProgramEnvironment where

import Com.NoSyn.Environment.AliasEnvironment
import Com.NoSyn.Environment.FunctionEnvironment
import Com.NoSyn.Environment.VariableEnvironment
import Com.NoSyn.Data.Types
import Com.NoSyn.Data.Variable
import Com.NoSyn.Error.CompilerStatus
import Data.Set.SetTheory as SetTH
import Com.Data.Map
import Com.Data.Map.Ordered
import Data.Map.Ordered as OrderMap
import Data.Map as Map

emptyProgramEnvironment = PE {
    aliases = emptyAliasEnvironment,
    functions = emptyFunctionEnvironment,
    variables = emptyVariableEnvironment,
    scopeReturnType = "Nothing" }
defaultProgramEnvironment = PE {
    aliases = defaultAliasEnvironment,
    functions = defaultFunctionEnvironment,
    variables = defaultVariableEnvironment,
    scopeReturnType = "Nothing" }

data ProgramEnvironment = PE { aliases :: AliasEnvironment,
                               functions :: FunctionEnvironment,
                               variables :: VariableEnvironment,
                               scopeReturnType :: Ident
                                             } deriving Show

instance SetSatisfiable ProgramEnvironment where
    union pe1@(PE {aliases = ae1, functions = fe1, variables = ve1}) (PE {aliases = ae2, functions = fe2, variables = ve2}) =
        let aeu = SetTH.union ae1 ae2 in
        let feu = SetTH.union fe1 fe2 in
        let veu = SetTH.union ve1 ve2 in
        pe1 {aliases = aeu, functions = feu, variables = veu}
    difference pe1@(PE {aliases = ae1, functions = fe1, variables = ve1}) (PE {aliases = ae2, functions = fe2, variables = ve2}) =
        let aeu = SetTH.difference ae1 ae2 in
        let feu = SetTH.difference fe1 fe2 in
        let veu = SetTH.difference ve1 ve2 in
        (pe1 {aliases = aeu, functions = feu, variables = veu})

instance MutuallyExcludable ProgramEnvironment where
    isEmpty pe =
        (SetTH.isEmpty (aliases pe)) && (SetTH.isEmpty (functions pe)) && (SetTH.isEmpty (variables pe))

reverseTake x = (Prelude.take x).reverse
reverseDrop x = reverse.(Prelude.drop x).reverse
lastChar = reverseTake 1
dropLastChar = reverseDrop 1

lookupDType::Ident->AliasEnvironment->CompilerStatus Ident
lookupDType noSynType aliasEnvironment
    | noSynType `OrderMap.member` aliasEnvironment = lookupDType' noSynType aliasEnvironment
    | lastChar noSynType == "*" && (dropLastChar noSynType) `OrderMap.member` aliasEnvironment = (lookupDType' (dropLastChar noSynType) aliasEnvironment)
    | reverseTake 3 noSynType == "..." && (reverseDrop 3 noSynType) `OrderMap.member` aliasEnvironment = (lookupDType' (reverseDrop 3 noSynType) aliasEnvironment)
    | otherwise = Error ("There is no type '" ++ noSynType ++ "' in scope") noSynType

lookupDType' noSynType aliasEnvironment
    | noSynType `OrderMap.member` aliasEnvironment = do
        nextAlias <- compilerStatusFromMaybe ("COMPILER ERROR: imported library incorrect") (OrderMap.lookup noSynType aliasEnvironment)
        if nextAlias == noSynType then return nextAlias -- If the aliases are the same then the break out of the loop because `nextAlias` is actually refering to a type in D
        else lookupDType' nextAlias aliasEnvironment
    | otherwise = return noSynType

lookupAtomicNoSynType::Ident->AliasEnvironment->CompilerStatus Ident
lookupAtomicNoSynType noSynType aliasEnvironment
    | noSynType `OrderMap.member` aliasEnvironment = do
        nextAlias <- compilerStatusFromMaybe ("COMPILER ERROR: imported library incorrect") (OrderMap.lookup noSynType aliasEnvironment)
        lookupAtomicNoSynType' noSynType nextAlias aliasEnvironment
    | lastChar noSynType == "*" && (dropLastChar noSynType) `OrderMap.member` aliasEnvironment = do
        nextAlias <- compilerStatusFromMaybe ("COMPILER ERROR: imported library incorrect") (OrderMap.lookup (dropLastChar noSynType) aliasEnvironment)
        lookupAtomicNoSynType' (dropLastChar noSynType) nextAlias aliasEnvironment >>= return
    | reverseTake 3 noSynType == "..." && (reverseDrop 3 noSynType) `OrderMap.member` aliasEnvironment = do
        nextAlias <- compilerStatusFromMaybe ("COMPILER ERROR: imported library incorrect") (OrderMap.lookup (reverseDrop 3 noSynType) aliasEnvironment)
        lookupAtomicNoSynType' (reverseDrop 3 noSynType) nextAlias aliasEnvironment >>= return
    | otherwise = (Error ("There is no type '" ++ noSynType ++ "' in scope") noSynType)

lookupAtomicNoSynType' previousType noSynType aliasEnvironment
    | noSynType `OrderMap.member` aliasEnvironment = do
        nextAlias <- compilerStatusFromMaybe ("COMPILER ERROR: imported library incorrect") (OrderMap.lookup noSynType aliasEnvironment)
        if noSynType == nextAlias then return noSynType -- If the aliases are the same then the break out of the loop because `nextAlias` is actually refering to a type in D
        else lookupAtomicNoSynType' noSynType nextAlias aliasEnvironment
    | otherwise = return previousType


lookupVariableType :: ProgramEnvironment -> Ident -> CompilerStatus Variable
lookupVariableType (PE { variables = variableEnvironment }) varName =
    compilerStatusFromMaybe ("There is no variable '" ++ varName ++ "' in scope") (Map.lookup varName variableEnvironment)
