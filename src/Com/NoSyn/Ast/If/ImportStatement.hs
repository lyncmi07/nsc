module Com.NoSyn.Ast.If.ImportStatement where

import Prelude hiding (getContents)
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.Traits.Blockable
import Com.NoSyn.Ast.If.Block
import Data.List
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Error.SourcePosition
import Com.NoSyn.Data.Types

type ImportStatements = Block ImportStatement
data ImportStatement =
    NativeImport [SourcePosition Ident]
    | NSImport [SourcePosition Ident]
    deriving Show

instance EnvironmentUpdater ImportStatement where
    updateEnvironment programEnvironment spImportStatement = case getContents spImportStatement of
        NativeImport _ -> return programEnvironment
        NSImport a -> PositionedError (getSourcePosition spImportStatement) "NoSyn imports are currently unsupported in this context" (show a)

instance TargetCodeGeneratable ImportStatement where
    generateD _ spImportStatement = case getContents spImportStatement of
        NativeImport moduleName ->
            let dModuleString = concat $ getContents $ sequence $ intersperse (return ".") moduleName in
              return $ "import " ++ dModuleString
        NSImport moduleIdentifiers -> return $ "import " ++ (concat $ getContents $ sequence $ intersperse (return ".") moduleIdentifiers)

instance Blockable ImportStatement where
    blockSeparator _ = ";\n"
