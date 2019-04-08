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

type ProgramEnvironment =
    (AliasEnvironment, FunctionEnvironment, VariableEnvironment)

instance SetSatisfiable ProgramEnvironment where
    union (ae1, fe1, ve1) (ae2, fe2, ve2) =
        let aeu = SetTH.union ae1 ae2 in
        let feu = SetTH.union fe1 fe2 in
        let veu = SetTH.union ve1 ve2 in
        (aeu, feu, veu)
    difference (ae1, fe1, ve1) (ae2, fe2, ve2) =
        let aeu = SetTH.difference ae1 ae2 in
        let feu = SetTH.difference fe1 fe2 in
        let veu = SetTH.difference ve1 ve2 in
        (aeu, feu, veu)
    size (ae, fe, ve) =
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
lookupVariableType (_, _, variableEnvironment) varName =
    compilerStatusFromMaybe ("There is no variable '" ++ varName ++ "' in scope") (Map.lookup varName variableEnvironment)
