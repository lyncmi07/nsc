module Com.NoSyn.Ast.If.ImportStatement where

import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Data.List
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Data.Types

type ImportStatements = Block ImportStatement
data ImportStatement =
    NativeImport [Ident]
    | NSImport [Ident]
    deriving Show

instance TargetCodeGeneratable ImportStatement where
    generateD _ (NativeImport moduleName) =
      let dModuleString = concat $ intersperse "." moduleName in
      return $ "import " ++ dModuleString
    generateD _ (NSImport _) = Error "NoSyn imports are not supported yet"

instance Blockable ImportStatement where
    blockSeparator = ";\n"
