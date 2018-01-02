{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module TaggedObjectNoTagSingleConstructors (tests) where

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
$(deriveJSON A.defaultOptions ''Unit)
$(deriveTypeScript A.defaultOptions ''Unit)

data OneFieldRecordless = OneFieldRecordless Int
$(deriveJSON A.defaultOptions ''OneFieldRecordless)
$(deriveTypeScript A.defaultOptions ''OneFieldRecordless)

data OneField = OneField { simpleString :: String }
$(deriveJSON A.defaultOptions ''OneField)
$(deriveTypeScript A.defaultOptions ''OneField)

data TwoFieldRecordless = TwoFieldRecordless Int String
$(deriveJSON A.defaultOptions ''TwoFieldRecordless)
$(deriveTypeScript A.defaultOptions ''TwoFieldRecordless)

data TwoField = TwoField { doubleInt :: Int
                         , doubleString :: String }
$(deriveJSON A.defaultOptions ''TwoField)
$(deriveTypeScript A.defaultOptions ''TwoField)

tests = unsafePerformIO $ testSpec "Token tests" $ do

  describe "TaggedObject" $ do
    describe "single constructor with tagSingleConstructors=False" $ do
      it [i|with a single nullary constructor like #{A.encode Unit}|] $ do
        (getTypeScriptType :: Tagged Unit String) `shouldBe` "Unit"
        (getTypeScriptDeclaration :: Tagged Unit [TSDeclaration]) `shouldBe` (Tagged [
          TSTypeAlternatives {typeName = "Unit", typeGenericVariables = [], alternativeTypes = ["void[]"]}
          ])

      it [i|with a single non-record constructor like #{A.encode $ OneFieldRecordless 42}|] $ do
        (getTypeScriptType :: Tagged OneFieldRecordless String) `shouldBe` "OneFieldRecordless"
        (getTypeScriptDeclaration :: Tagged OneFieldRecordless [TSDeclaration]) `shouldBe` (Tagged $ [
          TSTypeAlternatives "OneFieldRecordless" [] ["number"]
          ])

      it [i|with a single record constructor like #{A.encode $ OneField "asdf"}|] $ do
        (getTypeScriptType :: Tagged OneField String) `shouldBe` "OneField"
        (getTypeScriptDeclaration :: Tagged OneField [TSDeclaration]) `shouldBe` (Tagged [
          TSTypeAlternatives {typeName = "OneField", typeGenericVariables = [], alternativeTypes = ["IOneField"]},
          TSInterfaceDeclaration {interfaceName = "IOneField", interfaceGenericVariables = [], interfaceMembers = [
                                     TSField {fieldOptional = False, fieldName = "simpleString", fieldType = "string"}
                                     ]
                                 }
          ])

      it [i|with a two-field non-record constructor like #{A.encode $ TwoFieldRecordless 42 "asdf"}|] $ do
        (getTypeScriptType :: Tagged TwoFieldRecordless String) `shouldBe` "TwoFieldRecordless"
        (getTypeScriptDeclaration :: Tagged TwoFieldRecordless [TSDeclaration]) `shouldBe` (Tagged [
          TSTypeAlternatives "TwoFieldRecordless" [] ["[number, string]"]
          ])

      it [i|with a two-field record constructor like #{A.encode $ TwoField 42 "asdf"}|] $ do
        (getTypeScriptType :: Tagged TwoField String) `shouldBe` "TwoField"
        (getTypeScriptDeclaration :: Tagged TwoField [TSDeclaration]) `shouldBe` (Tagged [
          TSTypeAlternatives {typeName = "TwoField", typeGenericVariables = [], alternativeTypes = ["ITwoField"]},
          TSInterfaceDeclaration {interfaceName = "ITwoField", interfaceGenericVariables = [], interfaceMembers = [
                                     TSField {fieldOptional = False, fieldName = "doubleInt", fieldType = "number"},
                                     TSField {fieldOptional = False, fieldName = "doubleString", fieldType = "string"}
                                     ]
                                 }
          ])


main = defaultMainWithIngredients defaultIngredients tests
