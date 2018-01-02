{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module TaggedObjectTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.String.Interpolate.IsString
import Data.Tagged
import Prelude hiding (Double)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.Runners

data Unit = Unit
$(deriveJSON (A.defaultOptions {tagSingleConstructors=True}) ''Unit)
$(deriveTypeScript (A.defaultOptions {tagSingleConstructors=True}) ''Unit)

data OneFieldRecordless = OneFieldRecordless Int
$(deriveJSON (A.defaultOptions {tagSingleConstructors=True}) ''OneFieldRecordless)
$(deriveTypeScript (A.defaultOptions {tagSingleConstructors=True}) ''OneFieldRecordless)

data OneField = OneField { simpleString :: String }
$(deriveJSON (A.defaultOptions {tagSingleConstructors=True}) ''OneField)
$(deriveTypeScript (A.defaultOptions {tagSingleConstructors=True}) ''OneField)

data TwoFieldRecordless = TwoFieldRecordless Int String
$(deriveJSON (A.defaultOptions {tagSingleConstructors=True}) ''TwoFieldRecordless)
$(deriveTypeScript (A.defaultOptions {tagSingleConstructors=True}) ''TwoFieldRecordless)

data TwoField = TwoField { doubleInt :: Int
                         , doubleString :: String }
$(deriveJSON (A.defaultOptions {tagSingleConstructors=True}) ''TwoField)
$(deriveTypeScript (A.defaultOptions {tagSingleConstructors=True}) ''TwoField)

tests = unsafePerformIO $ testSpec "TaggedObject with tagSingleConstructors=True" $ do
  describe "single constructor" $ do
    it [i|with a single nullary constructor like #{A.encode Unit}|] $ do
      (getTypeScriptType :: Tagged Unit String) `shouldBe` "Unit"
      (getTypeScriptDeclaration :: Tagged Unit [TSDeclaration]) `shouldBe` (Tagged [
        TSTypeAlternatives {typeName = "Unit", typeGenericVariables = [], alternativeTypes = ["\"Unit\""]}
        ])

    it [i|with a single non-record constructor like #{A.encode $ OneFieldRecordless 42}|] $ do
      (getTypeScriptType :: Tagged OneFieldRecordless String) `shouldBe` "OneFieldRecordless"
      (getTypeScriptDeclaration :: Tagged OneFieldRecordless [TSDeclaration]) `shouldBe` (Tagged [
        TSTypeAlternatives {typeName = "OneFieldRecordless", typeGenericVariables = [], alternativeTypes = ["IOneFieldRecordless"]},
        TSInterfaceDeclaration {interfaceName = "IOneFieldRecordless", interfaceGenericVariables = [], interfaceMembers = [
                                   TSField {fieldOptional = False, fieldName = "tag", fieldType = "string"},
                                   TSField {fieldOptional = False, fieldName = "contents", fieldType = "number"}
                                   ]
                               }
        ])

    it [i|with a single record constructor like #{A.encode $ OneField "asdf"}|] $ do
      (getTypeScriptType :: Tagged OneField String) `shouldBe` "OneField"
      (getTypeScriptDeclaration :: Tagged OneField [TSDeclaration]) `shouldBe` (Tagged [
        TSTypeAlternatives {typeName = "OneField", typeGenericVariables = [], alternativeTypes = ["IOneField"]},
        TSInterfaceDeclaration {interfaceName = "IOneField", interfaceGenericVariables = [], interfaceMembers = [
                                   TSField {fieldOptional = False, fieldName = "tag", fieldType = "string"},
                                   TSField {fieldOptional = False, fieldName = "simpleString", fieldType = "string"}
                                   ]
                               }
        ])

    it [i|with a two-field non-record constructor like #{A.encode $ TwoFieldRecordless 42 "asdf"}|] $ do
      (getTypeScriptType :: Tagged TwoFieldRecordless String) `shouldBe` "TwoFieldRecordless"
      (getTypeScriptDeclaration :: Tagged TwoFieldRecordless [TSDeclaration]) `shouldBe` (Tagged [
        TSTypeAlternatives {typeName = "TwoFieldRecordless", typeGenericVariables = [], alternativeTypes = ["ITwoFieldRecordless"]},
        TSInterfaceDeclaration {interfaceName = "ITwoFieldRecordless", interfaceGenericVariables = [], interfaceMembers = [
                                   TSField {fieldOptional = False, fieldName = "tag", fieldType = "string"},
                                   TSField {fieldOptional = False, fieldName = "contents", fieldType = "[number, string]"}
                                   ]
                               }
        ])

    it [i|with a two-field record constructor like #{A.encode $ TwoField 42 "asdf"}|] $ do
      (getTypeScriptType :: Tagged TwoField String) `shouldBe` "TwoField"
      (getTypeScriptDeclaration :: Tagged TwoField [TSDeclaration]) `shouldBe` (Tagged [
        TSTypeAlternatives {typeName = "TwoField", typeGenericVariables = [], alternativeTypes = ["ITwoField"]},
        TSInterfaceDeclaration {interfaceName = "ITwoField", interfaceGenericVariables = [], interfaceMembers = [
                                   TSField {fieldOptional = False, fieldName = "tag", fieldType = "string"},
                                   TSField {fieldOptional = False, fieldName = "doubleInt", fieldType = "number"},
                                   TSField {fieldOptional = False, fieldName = "doubleString", fieldType = "string"}
                                   ]
                               }
        ])


main = defaultMainWithIngredients defaultIngredients tests
