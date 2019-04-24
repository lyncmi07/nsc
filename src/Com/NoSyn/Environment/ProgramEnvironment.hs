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
import Com.SetTheory.SetSatisfiable as SetTH
import Com.Data.Map
import Com.Data.Map.Ordered
import Data.Map.Ordered as OrderMap
import Data.Map as Map

emptyProgramEnvironment =
    (emptyAliasEnvironment, emptyFunctionEnvironment, emptyVariableEnvironment)
defaultProgramEnvironment =
    (defaultAliasEnvironment, defaultFunctionEnvironment, defaultVariableEnvironment)

data ProgramEnvironment = PG { aliases :: AliasEnvironment,
                                               functions :: FunctionEnvironment,
                                               variables :: VariableEnvironment
                                             } deriving Show

instance SetSatisfiable ProgramEnvironment where
    union (PG {aliases = ae1, functions = fe1, variables = ve1}) (PG {aliases = ae2, functions = fe2, variables = ve2}) =
        let aeu = SetTH.union ae1 ae2 in
        let feu = SetTH.union fe1 fe2 in
        let veu = SetTH.union ve1 ve2 in
        PG {aliases = aeu, functions = feu, variables = veu}
    difference (PG {aliases = ae1, functions = fe1, variables = ve1}) (PG {aliases = ae2, functions = fe2, variables = ve2}) =
        let aeu = SetTH.difference ae1 ae2 in
        let feu = SetTH.difference fe1 fe2 in
        let veu = SetTH.difference ve1 ve2 in
        (PG {aliases = aeu, functions = feu, variables = veu})
    size (PG {aliases = ae, functions = fe, variables = ve}) =
        let aeu = SetTH.size ae in
        let feu = SetTH.size fe in
        let veu = SetTH.size ve in
        aeu + feu + veu

lookupDType::Ident->AliasEnvironment->CompilerStatus Ident
lookupDType noSynType aliasEnvironment
    | noSynType `OrderMap.member` aliasEnvironment = lookupDType' noSynType aliasEnvironment
    | otherwise = Error $ "There is no type '" ++ noSynType ++ "' in scope"

lookupDType' noSynType aliasEnvironment
    | noSynType `OrderMap.member` aliasEnvironment = do
        nextAlias <- compilerStatusFromMaybe ("COMPILER ERROR: imported library incorrect") (OrderMap.lookup noSynType aliasEnvironment)
        lookupDType' nextAlias aliasEnvironment
    | otherwise = return noSynType

lookupAtomicNoSynType::Ident->AliasEnvironment->CompilerStatus Ident
lookupAtomicNoSynType noSynType aliasEnvironment
    | noSynType `OrderMap.member` aliasEnvironment = do
      nextAlias <- compilerStatusFromMaybe ("COMPILER ERROR: imported library incorrect") (OrderMap.lookup noSynType aliasEnvironment)
      lookupAtomicNoSynType' noSynType nextAlias aliasEnvironment
    | otherwise = Error $ "There is no type '" ++ noSynType ++ "' in scope"

lookupAtomicNoSynType' previousType noSynType aliasEnvironment
    | noSynType `OrderMap.member` aliasEnvironment = do
        nextAlias <- compilerStatusFromMaybe ("COMPILER ERROR: imported library incorrect") (OrderMap.lookup noSynType aliasEnvironment)
        lookupAtomicNoSynType' noSynType nextAlias aliasEnvironment
    | otherwise = return previousType


lookupVariableType :: ProgramEnvironment -> Ident -> CompilerStatus Variable
lookupVariableType (PG { variables = variableEnvironment }) varName =
    compilerStatusFromMaybe ("There is no variable '" ++ varName ++ "' in scope") (Map.lookup varName variableEnvironment)
