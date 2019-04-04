module Com.NoSyn.Ast.If.Block where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.Listable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Data.List
import Com.SetTheory.SetSatisfiable
import Control.Monad
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Environment.ProgramEnvironment
import Com.HigherOrderFunctions

class (EnvironmentUpdater m) => Blockable m where
    blockSeparator :: m->String

data Block a =
    StandardBlock [a]
    | SequentialBlock [a]
    deriving Show

instance (EnvironmentUpdater a) => EnvironmentUpdater (Block a) where
    updateEnvironment programEnvironment (StandardBlock m) = do
        updatedProgramEnvironments <- sequence $ map (updateEnvironment programEnvironment) m
        let differenceProgramEnvironments = map (\x -> difference x programEnvironment) updatedProgramEnvironments in
            foldM (concatenate (Error "Cannot override program environment with new definitions")) programEnvironment differenceProgramEnvironments
    updateEnvironment programEnvironment (SequentialBlock m) =
        foldM updateEnvironment programEnvironment m

instance (EnvironmentUpdater a, TargetCodeGeneratable a, Blockable a) => TargetCodeGeneratable (Block a) where
    generateD programEnvironment (StandardBlock a) = do
        generatedElements <- sequence $ map (generateD programEnvironment)  a
        return $ 
            concat $ 
                intersperse (blockSeparator (head a)) generatedElements
    generateD programEnvironment (SequentialBlock a) = do
        generatedElements <- environmentUpdatingMapMonad updateEnvironment generateD programEnvironment a
        return $ 
            concat $ 
                intersperse (blockSeparator (head a)) generatedElements

instance Listable Block where
    toList (StandardBlock a) = a
    toList (SequentialBlock  a) = a
