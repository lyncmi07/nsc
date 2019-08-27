module Com.NoSyn.Evaluation.Program.Internal.AliasEvaluation (programAliasEvaluate) where

import Com.NoSyn.Ast.If.Program
import Com.NoSyn.Ast.If.AliasDefinition
import Com.NoSyn.Ast.Traits.Listable as Listable
import Com.NoSyn.Error.CompilerStatus
import Data.Map.Ordered
import Data.Set
import Com.NoSyn.Environment.AliasEnvironment
import Com.NoSyn.Data.Types

programAliasEvaluate::AliasEnvironment -> Program -> CompilerStatus AliasEnvironment
programAliasEvaluate defaultEnvironment program =
    let environmentWithNativeAliases = addNativeAliases (Listable.toList program) defaultEnvironment in
    let noSynLookupTable = createNoSynTypeLookupTable (Listable.toList program) in
    createRealTypeLookupTable noSynLookupTable environmentWithNativeAliases

addNativeAliases::[ProgramStmt] -> AliasEnvironment -> AliasEnvironment
addNativeAliases [] aliasEnvironment = aliasEnvironment
addNativeAliases ((PSAliasDef (ADNative aliasName aliasType)):xs) aliasEnvironment =
    let updatedAliasEnvironment = (aliasName, aliasType) |< aliasEnvironment in
    addNativeAliases xs updatedAliasEnvironment
addNativeAliases (_:xs) aliasEnvironment = addNativeAliases xs aliasEnvironment

createNoSynTypeLookupTable::[ProgramStmt] -> OMap Ident Ident
createNoSynTypeLookupTable [] = Data.Map.Ordered.empty
createNoSynTypeLookupTable ((PSAliasDef (ADNoSyn aliasName aliasType)):xs) =
    (aliasName, aliasType) |< (createNoSynTypeLookupTable xs)
createNoSynTypeLookupTable (_:xs) = createNoSynTypeLookupTable xs

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
    | otherwise = Error ("'alias " ++ aliasName ++ " = " ++ aliasType ++ "' is an invalid alias") "Context given"
    where
        ((aliasName, aliasType):xs) = Data.Map.Ordered.assocs noSynLookupTable

keySet::Ord a=>OMap a b -> Set a
keySet orderedMap = Prelude.foldl (\x (y,_)->Data.Set.insert y x) Data.Set.empty (Data.Map.Ordered.assocs orderedMap)
