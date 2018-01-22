{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module ObjectWithSingleFieldTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import Prelude hiding (Double)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.Runners
import Util


data Unit = Unit
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''Unit)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''Unit)

data OneFieldRecordless = OneFieldRecordless Int
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''OneFieldRecordless)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''OneFieldRecordless)

data OneField = OneField { simpleString :: String }
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''OneField)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''OneField)

data TwoFieldRecordless = TwoFieldRecordless Int String
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''TwoFieldRecordless)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''TwoFieldRecordless)

data TwoField = TwoField { doubleInt :: Int
                         , doubleString :: String }
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''TwoField)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''TwoField)

data TwoConstructor = Con1 { con1String :: String }
                    | Con2 { con2String :: String
                           , con2Int :: Int }
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''TwoConstructor)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField, tagSingleConstructors=True}) ''TwoConstructor)


tests = unsafePerformIO $ testSpec "ObjectWithSingleField with tagSingleConstructors=True" $ do
  describe "single constructor" $ do
    -- it [i|with a single nullary constructor like #{A.encode Unit}|] $ do
    --   (getTypeScriptType (Proxy :: Proxy Unit)) `shouldBe` "Unit"
    --   (getTypeScriptDeclaration (Proxy :: Proxy Unit)) `shouldBe` ([
    --     TSTypeAlternatives {typeName = "Unit", typeGenericVariables = [], alternativeTypes = ["IUnit"]},
    --     TSTypeAlternatives {typeName = "IUnit", typeGenericVariables = [], alternativeTypes = ["\"Unit\""]}
    --     ])

    -- it [i|with a single non-record constructor like #{A.encode $ OneFieldRecordless 42}|] $ do
    --   (getTypeScriptType (Proxy :: Proxy OneFieldRecordless)) `shouldBe` "OneFieldRecordless"
    --   (getTypeScriptDeclaration (Proxy :: Proxy OneFieldRecordless)) `shouldBe` ([
    --     TSTypeAlternatives {typeName = "OneFieldRecordless", typeGenericVariables = [], alternativeTypes = ["{\"OneFieldRecordless\": IOneFieldRecordless}"]},
    --     TSTypeAlternatives {typeName = "IOneFieldRecordless", typeGenericVariables = [], alternativeTypes = ["number"]}
    --     ])

    -- it [i|with a single record constructor like #{A.encode $ OneField "asdf"}|] $ do
    --   (getTypeScriptType (Proxy :: Proxy OneField)) `shouldBe` "OneField"
    --   (getTypeScriptDeclaration (Proxy :: Proxy OneField)) `shouldBe` ([
    --     TSTypeAlternatives {typeName = "OneField", typeGenericVariables = [], alternativeTypes = ["{\"OneField\": IOneField}"]},
    --     TSInterfaceDeclaration {interfaceName = "IOneField", interfaceGenericVariables = [], interfaceMembers = [TSField {fieldOptional = False, fieldName = "simpleString", fieldType = "string"}]}
    --     ])

    -- it [i|with a two-field non-record constructor like #{A.encode $ TwoFieldRecordless 42 "asdf"}|] $ do
    --   (getTypeScriptType (Proxy :: Proxy TwoFieldRecordless)) `shouldBe` "TwoFieldRecordless"
    --   (getTypeScriptDeclaration (Proxy :: Proxy TwoFieldRecordless)) `shouldBe` ([
    --     TSTypeAlternatives {typeName = "TwoFieldRecordless", typeGenericVariables = [], alternativeTypes = ["{\"TwoFieldRecordless\": ITwoFieldRecordless}"]},
    --     TSTypeAlternatives {typeName = "ITwoFieldRecordless", typeGenericVariables = [], alternativeTypes = ["[number, string]"]}])

    -- it [i|with a two-field record constructor like #{A.encode $ TwoField 42 "asdf"}|] $ do
    --   (getTypeScriptType (Proxy :: Proxy TwoField)) `shouldBe` "TwoField"
    --   (getTypeScriptDeclaration (Proxy :: Proxy TwoField)) `shouldBe` ([
    --     TSTypeAlternatives {typeName = "TwoField", typeGenericVariables = [], alternativeTypes = ["{\"TwoField\": ITwoField}"]},
    --     TSInterfaceDeclaration {interfaceName = "ITwoField", interfaceGenericVariables = [], interfaceMembers = [TSField {fieldOptional = False, fieldName = "doubleInt", fieldType = "number"},TSField {fieldOptional = False, fieldName = "doubleString", fieldType = "string"}]}
    --     ])

    -- it [i|with a two-constructor type like #{A.encode $ Con1 "asdf"} or #{A.encode $ Con2 "asdf" 42}|] $ do
    --   (getTypeScriptType (Proxy :: Proxy TwoConstructor)) `shouldBe` "TwoConstructor"
    --   (getTypeScriptDeclaration (Proxy :: Proxy TwoConstructor)) `shouldBe` ([
    --     TSTypeAlternatives {typeName = "TwoConstructor", typeGenericVariables = [], alternativeTypes = ["{\"Con1\": ICon1}","{\"Con2\": ICon2}"]},
    --     TSInterfaceDeclaration {interfaceName = "ICon1", interfaceGenericVariables = [], interfaceMembers = [TSField {fieldOptional = False, fieldName = "con1String", fieldType = "string"}]},
    --     TSInterfaceDeclaration {interfaceName = "ICon2", interfaceGenericVariables = [], interfaceMembers = [TSField {fieldOptional = False, fieldName = "con2String", fieldType = "string"},TSField {fieldOptional = False, fieldName = "con2Int", fieldType = "number"}]}
    --     ])

    it "type checks everything with tsc" $ do
      let declarations = ((getTypeScriptDeclaration (Proxy :: Proxy Unit)) <>
                          (getTypeScriptDeclaration (Proxy :: Proxy OneFieldRecordless)) <>
                          (getTypeScriptDeclaration (Proxy :: Proxy OneField)) <>
                          (getTypeScriptDeclaration (Proxy :: Proxy TwoFieldRecordless)) <>
                          (getTypeScriptDeclaration (Proxy :: Proxy TwoField)) <>
                          (getTypeScriptDeclaration (Proxy :: Proxy TwoConstructor))
                         )

      let typesAndValues = [(getTypeScriptType (Proxy :: Proxy Unit) , A.encode Unit)
                           , (getTypeScriptType (Proxy :: Proxy OneFieldRecordless) , A.encode $ OneFieldRecordless 42)
                           , (getTypeScriptType (Proxy :: Proxy OneField) , A.encode $ OneField "asdf")
                           , (getTypeScriptType (Proxy :: Proxy TwoFieldRecordless) , A.encode $ TwoFieldRecordless 42 "asdf")
                           , (getTypeScriptType (Proxy :: Proxy TwoField) , A.encode $ TwoField 42 "asdf")
                           , (getTypeScriptType (Proxy :: Proxy TwoConstructor) , A.encode $ Con1 "asdf")
                           , (getTypeScriptType (Proxy :: Proxy TwoConstructor) , A.encode $ Con2 "asdf" 42)
                           ]

      testTypeCheckDeclarations declarations typesAndValues


main = defaultMainWithIngredients defaultIngredients tests

main' = putStrLn $ formatTSDeclarations (
   (getTypeScriptDeclaration (Proxy :: Proxy Unit)) <>
   (getTypeScriptDeclaration (Proxy :: Proxy OneFieldRecordless)) <>
   (getTypeScriptDeclaration (Proxy :: Proxy OneField)) <>
   (getTypeScriptDeclaration (Proxy :: Proxy TwoFieldRecordless)) <>
   (getTypeScriptDeclaration (Proxy :: Proxy TwoField)) <>
   (getTypeScriptDeclaration (Proxy :: Proxy TwoConstructor))
  )
