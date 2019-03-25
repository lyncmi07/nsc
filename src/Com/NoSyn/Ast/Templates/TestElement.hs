{-# LANGUAGE TemplateHaskell #-}
module Com.NoSyn.Ast.Templates.TestElement where

import Com.NoSyn.Ast.Templates.IfElement
import Com.NoSyn.Error.CompilerStatus
import Com.NoSyn.Ast.Traits.TargetCodeGeneratable
import Com.NoSyn.Environment.ProgramEnvironment

data TestElement a =
    RealElement
    | FakeElement
    | Ifm1Element a

createIfElement ''TestElement "TestElement"
instance IfTestElement a => TargetCodeGeneratable (TestElement a) where
    generateD _ RealElement = return "real element"
    generateD _ FakeElement = return "fake element"
    generateD b c = $(ifm1GeneratorExpression 'b 'c "TestElement")
    
instance IfTestElement TestElement2 where
    generateIfTestElement _ RealElement1 = return RealElement
    generateIfTestElement _ RealElement2 = return RealElement
    generateIfTestElement _ FakeElement1 = return RealElement

data TestElement2 = 
    RealElement1
    | RealElement2
    | FakeElement1
