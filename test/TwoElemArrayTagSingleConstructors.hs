{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module TwoElemArrayTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Monoid
import Data.String.Interpolate.IsString
import Data.Tagged
import Prelude hiding (Double)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.Runners

data Unit = Unit
$(deriveJSON (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''Unit)
$(deriveTypeScript (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''Unit)

data OneFieldRecordless = OneFieldRecordless Int
$(deriveJSON (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''OneFieldRecordless)
$(deriveTypeScript (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''OneFieldRecordless)

data OneField = OneField { simpleString :: String }
$(deriveJSON (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''OneField)
$(deriveTypeScript (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''OneField)

data TwoFieldRecordless = TwoFieldRecordless Int String
$(deriveJSON (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''TwoFieldRecordless)
$(deriveTypeScript (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''TwoFieldRecordless)

data TwoField = TwoField { doubleInt :: Int
                         , doubleString :: String }
$(deriveJSON (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''TwoField)
$(deriveTypeScript (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''TwoField)

data TwoConstructor = Con1 { con1String :: String }
                    | Con2 { con2String :: String
                           , con2Int :: Int }
$(deriveJSON (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''TwoConstructor)
$(deriveTypeScript (A.defaultOptions {sumEncoding=TwoElemArray, tagSingleConstructors=True}) ''TwoConstructor)


tests = unsafePerformIO $ testSpec "TwoElemArray with tagSingleConstructors=False" $ do
  describe "single constructor" $ do
    it [i|with a single nullary constructor like #{A.encode Unit}|] $ do
      (getTypeScriptType :: Tagged Unit String) `shouldBe` "Unit"
      (getTypeScriptDeclaration :: Tagged Unit [TSDeclaration]) `shouldBe` (Tagged [
        TSTypeAlternatives "Unit" [] [[i|"Unit"|]]
        ])

    it [i|with a single non-record constructor like #{A.encode $ OneFieldRecordless 42}|] $ do
      (getTypeScriptType :: Tagged OneFieldRecordless String) `shouldBe` "OneFieldRecordless"
      (getTypeScriptDeclaration :: Tagged OneFieldRecordless [TSDeclaration]) `shouldBe` (Tagged $ [
        TSTypeAlternatives "OneFieldRecordless" [] ["[string, number]"]
        ])

    it [i|with a single record constructor like #{A.encode $ OneField "asdf"}|] $ do
      (getTypeScriptType :: Tagged OneField String) `shouldBe` "OneField"
      (getTypeScriptDeclaration :: Tagged OneField [TSDeclaration]) `shouldBe` (Tagged [
        TSTwoElemArray "OneField" [] ["IOneField"],
        TSInterfaceDeclaration "IOneField" [] [TSField False "simpleString" "string"]
        ])

    it [i|with a two-field non-record constructor like #{A.encode $ TwoFieldRecordless 42 "asdf"}|] $ do
      (getTypeScriptType :: Tagged TwoFieldRecordless String) `shouldBe` "TwoFieldRecordless"
      (getTypeScriptDeclaration :: Tagged TwoFieldRecordless [TSDeclaration]) `shouldBe` (Tagged [
        TSTypeAlternatives "TwoFieldRecordless" [] ["[number, string]"]
        ])

    it [i|with a two-field record constructor like #{A.encode $ TwoField 42 "asdf"}|] $ do
      (getTypeScriptType :: Tagged TwoField String) `shouldBe` "TwoField"
      (getTypeScriptDeclaration :: Tagged TwoField [TSDeclaration]) `shouldBe` (Tagged [
        TSTwoElemArray "TwoField" [] ["ITwoField"],
        TSInterfaceDeclaration "ITwoField" [] [TSField False "doubleInt" "number",
                                               TSField False "doubleString" "string"]
        ])

    it [i|with a two-constructor type like #{A.encode $ Con1 "asdf"} or #{A.encode $ Con2 "asdf" 42}|] $ do
      (getTypeScriptType :: Tagged TwoConstructor String) `shouldBe` "TwoConstructor"
      (getTypeScriptDeclaration :: Tagged TwoConstructor [TSDeclaration]) `shouldBe` (Tagged [
        TSTwoElemArray {typeName = "TwoConstructor", typeGenericVariables = [], alternativeTypes = ["ICon1","ICon2"]},
        TSInterfaceDeclaration "ICon1" [] [TSField False "con1String" "string"],
        TSInterfaceDeclaration "ICon2" [] [TSField False "con2String" "string",
                                           TSField False "con2Int" "number"]
        ])


main = defaultMainWithIngredients defaultIngredients tests

main' = putStrLn $ formatTSDeclarations (
  unTagged (getTypeScriptDeclaration :: Tagged Unit [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged OneFieldRecordless [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged OneField [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged TwoFieldRecordless [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged TwoField [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged TwoConstructor [TSDeclaration])
  )
