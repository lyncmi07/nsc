module Com.NoSyn.Ast.If.ImportStatement where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Ast.Traits.EnvironmentUpdater
import Com.NoSyn.Ast.If.Block
import Data.List
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Data.Types

type ImportStatements = Block ImportStatement
data ImportStatement =
    NativeImport [Ident]
    | NSImport [Ident]
    deriving Show

instance EnvironmentUpdater ImportStatement where
    updateEnvironment programEnvironment (NativeImport _) = return programEnvironment
    updateEnvironment programEnvironment (NSImport a) = Error "NoSyn imports are currently unsupported in this context" (show a)

instance TargetCodeGeneratable ImportStatement where
    generateD _ (NativeImport moduleName) =
      let dModuleString = concat $ intersperse "." moduleName in
      return $ "import " ++ dModuleString
    generateD _ (NSImport moduleIdentifiers) = return $ "import " ++ (concat $ intersperse "." moduleIdentifiers)

instance Blockable ImportStatement where
    blockSeparator _ = ";\n"
