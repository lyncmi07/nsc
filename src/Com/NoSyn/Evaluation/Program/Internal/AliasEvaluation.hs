module Com.NoSyn.Evaluation.Program.Internal.AliasEvaluation (programAliasEvaluate) where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.If.Program
import Com.NoSyn.Ast.If.AliasDefinition
import Com.NoSyn.Ast.Traits.Listable as Listable
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Error.SourcePositionTraits
import Data.Map.Ordered
import Data.Set
import Com.NoSyn.Environment.AliasEnvironment
import Com.NoSyn.Data.Types

programAliasEvaluate::AliasEnvironment -> SourcePosition Program -> CompilerStatus AliasEnvironment
programAliasEvaluate defaultEnvironment program =
    let environmentWithNativeAliases = addNativeAliases (sourcePositionToList program) defaultEnvironment in
    let noSynLookupTable = createNoSynTypeLookupTable (sourcePositionToList program) in
    createRealTypeLookupTable noSynLookupTable environmentWithNativeAliases

addNativeAliases::[SourcePosition ProgramStmt] -> AliasEnvironment -> AliasEnvironment
addNativeAliases [] aliasEnvironment = aliasEnvironment
addNativeAliases (spProgramStmt:xs) aliasEnvironment = case getContents spProgramStmt of
    PSAliasDef aliasDef -> case getContents aliasDef of
        ADNative aliasName aliasType ->
            let updatedAliasEnvironment = (aliasName, aliasType) |< aliasEnvironment in
                addNativeAliases xs updatedAliasEnvironment
    otherwise -> addNativeAliases xs aliasEnvironment
    
createNoSynTypeLookupTable::[SourcePosition ProgramStmt] -> OMap Ident Ident
createNoSynTypeLookupTable [] = Data.Map.Ordered.empty
createNoSynTypeLookupTable (spProgramStmt:xs) = case getContents spProgramStmt of
    PSAliasDef aliasDef -> case getContents aliasDef of
        ADNoSyn aliasName aliasType ->
            (aliasName, aliasType) |< (createNoSynTypeLookupTable xs)
        otherwise -> createNoSynTypeLookupTable xs

createRealTypeLookupTable::OMap Ident Ident -> AliasEnvironment -> CompilerStatus AliasEnvironment
createRealTypeLookupTable noSynLookupTable realLookupTable
    | noSynLookupTable == Data.Map.Ordered.empty = return realLookupTable
createRealTypeLookupTable noSynLookupTable realLookupTable
    | Data.Set.member aliasType (keySet realLookupTable) = do
        createRealTypeLookupTable (Data.Map.Ordered.fromList xs) ((aliasName, aliasType) |< realLookupTable)
    | Data.Set.member
        aliasType 
        (Data.Set.union
            (keySet realLookupTable)
            (keySet noSynLookupTable)) =
                let reorderedNoSynLookupTable = Data.Map.Ordered.fromList (xs ++ [(aliasName, aliasType)]) in
                createRealTypeLookupTable reorderedNoSynLookupTable realLookupTable
    | otherwise = Error ("'alias " ++ aliasName ++ " = " ++ aliasType ++ "' is an invalid alias") (show (noSynLookupTable, realLookupTable))
    where
        ((aliasName, aliasType):xs) = Data.Map.Ordered.assocs noSynLookupTable

keySet::Ord a=>OMap a b -> Set a
keySet orderedMap = Prelude.foldl (\x (y,_)->Data.Set.insert y x) Data.Set.empty (Data.Map.Ordered.assocs orderedMap)
