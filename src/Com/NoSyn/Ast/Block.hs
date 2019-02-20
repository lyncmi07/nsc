module Com.NoSyn.Ast.Block where

import Com.NoSyn.Ast.AstElement
import Com.NoSyn.Ast.Listable
import Com.NoSyn.Ast.EnvironmentUpdater
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

instance (EnvironmentUpdater a) => EnvironmentUpdater (Block a) where
    updateEnvironment programEnvironment (StandardBlock m) = do
        updatedProgramEnvironments <- sequence $ map (updateEnvironment programEnvironment) m
        let differenceProgramEnvironments = map (\x -> difference x programEnvironment) updatedProgramEnvironments in
            foldM (concatenate (Error "Cannot override program environment with new definitions")) programEnvironment differenceProgramEnvironments
    updateEnvironment programEnvironment (SequentialBlock m) =
        foldM updateEnvironment programEnvironment m


instance (EnvironmentUpdater a, AstElement a, Blockable a) => AstElement (Block a) where
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
