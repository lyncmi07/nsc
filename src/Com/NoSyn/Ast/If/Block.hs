module Com.NoSyn.Ast.If.Block where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.Listable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.Blockable
import Data.List
import Data.Set.SetTheory
import Control.Monad
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.SourcePositionTraits
import Com.NoSyn.Environment.ProgramEnvironment
import Com.HigherOrderFunctions

data Block a =
    StandardBlock [SourcePosition a]
    | SequentialBlock [SourcePosition a]
    deriving Show

instance (EnvironmentUpdater a) => EnvironmentUpdater (Block a) where
    updateEnvironment programEnvironment spBlock = case getContents spBlock of
        StandardBlock m -> do
            updatedProgramEnvironments <- sequence $ map (updateEnvironment programEnvironment) m
            let differenceProgramEnvironments = map (\x -> difference x programEnvironment) updatedProgramEnvironments in
                foldM 
                    (\x y -> concatenate (PositionedError (getSourcePosition spBlock) "Cannot override program environment with new definitions" (show (x, y))) x y) 
                        programEnvironment 
                        differenceProgramEnvironments
        SequentialBlock m ->
            foldM updateEnvironment programEnvironment m

instance (EnvironmentUpdater a, TargetCodeGeneratable a, Blockable a) => TargetCodeGeneratable (Block a) where
    generateD programEnvironment spBlock = case getContents spBlock of
        StandardBlock a -> do
            generatedElements <- sequence $ map (generateD programEnvironment)  a
            return $ concat $ intersperse (blockSeparator (head a)) generatedElements
        SequentialBlock a -> do
            generatedElements <- environmentUpdatingMapMonad updateEnvironment generateD programEnvironment $ a
            return $ concat $ intersperse (blockSeparator (head a)) generatedElements

instance Listable Block where
    toList (StandardBlock a) = getContents $ sequence a
    toList (SequentialBlock  a) = getContents $ sequence a

toSourcePositionedList :: Block a -> [SourcePosition a]
toSourcePositionedList (StandardBlock l) = l
toSourcePositionedList (SequentialBlock l) = l

instance Functor Block where
    fmap f (StandardBlock a) = StandardBlock $ map (fmap f) a
    fmap f (SequentialBlock a) = SequentialBlock $ map (fmap f) a
