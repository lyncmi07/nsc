{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances #-}
module Com.NoSyn.Ast.Templates.TestElement where

import Com.NoSyn.Ast.Templates.IfElement
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Environment.ProgramEnvironment

data TestElement =
    RealElement
    | FakeElement

instance {-# OVERLAPPING #-} TargetCodeGeneratable TestElement where
    generateD _ RealElement = return "real element"
    generateD _ FakeElement = return "fake element"
createIfElement ''TestElement "TestElement"

data TestElement2 = 
    RealElement1
    | RealElement2
    | FakeElement1

instance IfTestElement TestElement2 where
    generateIfTestElement _ RealElement1 = return RealElement
    generateIfTestElement _ RealElement2 = return RealElement
    generateIfTestElement _ FakeElement1 = return RealElement
