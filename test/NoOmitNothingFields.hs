{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module NoOmitNothingFields (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Test.Hspec
import TestBoilerplate

$(testDeclarations "NoOmitNothingFields" (A.defaultOptions {omitNothingFields=False}))

main = hspec $ describe "NoOmitNothingFields" $ do
  it "encodes as expected" $ do
    let decls = getTypeScriptDeclarations (Proxy :: Proxy Optional)

    decls `shouldBe` [TSInterfaceDeclaration {
                         interfaceName = "Optional"
                         , interfaceGenericVariables = []
                         , interfaceMembers = [
                             TSField {fieldOptional = False
                                     , fieldName = "optionalInt"
                                     , fieldType = "number | null"}
                             ]
                         }]

  tests
