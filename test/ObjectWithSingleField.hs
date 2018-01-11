{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module Untagged (tests) where

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
import Util


data Unit = Unit
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''Unit)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''Unit)

data OneFieldRecordless = OneFieldRecordless Int
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''OneFieldRecordless)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''OneFieldRecordless)

data OneField = OneField { simpleString :: String }
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''OneField)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''OneField)

data TwoFieldRecordless = TwoFieldRecordless Int String
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoFieldRecordless)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoFieldRecordless)

data TwoField = TwoField { doubleInt :: Int
                         , doubleString :: String }
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoField)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoField)

data TwoConstructor = Con1 { con1String :: String }
                    | Con2 { con2String :: String
                           , con2Int :: Int }
$(deriveJSON (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoConstructor)
$(deriveTypeScript (A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoConstructor)


tests = unsafePerformIO $ testSpec "ObjectWithSingleField" $ do
  describe "single constructor" $ do
    it [i|with a single nullary constructor like #{A.encode Unit}|] $ do
      (getTypeScriptType :: Tagged Unit String) `shouldBe` "Unit"
      (getTypeScriptDeclaration :: Tagged Unit [TSDeclaration]) `shouldBe` (Tagged [
        TSTypeAlternatives "Unit" [] ["void[]"]
        ])

    it [i|with a single non-record constructor like #{A.encode $ OneFieldRecordless 42}|] $ do
      (getTypeScriptType :: Tagged OneFieldRecordless String) `shouldBe` "OneFieldRecordless"
      (getTypeScriptDeclaration :: Tagged OneFieldRecordless [TSDeclaration]) `shouldBe` (Tagged $ [
        TSTypeAlternatives "OneFieldRecordless" [] ["number"]
        ])

    it [i|with a single record constructor like #{A.encode $ OneField "asdf"}|] $ do
      (getTypeScriptType :: Tagged OneField String) `shouldBe` "OneField"
      (getTypeScriptDeclaration :: Tagged OneField [TSDeclaration]) `shouldBe` (Tagged [
        TSTypeAlternatives "OneField" [] ["IOneField"],
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
        TSTypeAlternatives "TwoField" [] ["ITwoField"],
        TSInterfaceDeclaration "ITwoField" [] [TSField False "doubleInt" "number",
                                               TSField False "doubleString" "string"]
        ])

    it [i|with a two-constructor type like #{A.encode $ Con1 "asdf"} or #{A.encode $ Con2 "asdf" 42}|] $ do
      (getTypeScriptType :: Tagged TwoConstructor String) `shouldBe` "TwoConstructor"
      (getTypeScriptDeclaration :: Tagged TwoConstructor [TSDeclaration]) `shouldBe` (Tagged [
        TSObjectWithSingleField "TwoConstructor" [] [("Con1","ICon1"),("Con2","ICon2")],
        TSInterfaceDeclaration "ICon1" [] [TSField False "con1String" "string"],
        TSInterfaceDeclaration "ICon2" [] [TSField False "con2String" "string",
                                           TSField False "con2Int" "number"]
        ])

    it "type checks everything with tsc" $ do
      let declarations = ((untag $ (getTypeScriptDeclaration :: Tagged Unit [TSDeclaration])) <>
                          (untag $ (getTypeScriptDeclaration :: Tagged OneFieldRecordless [TSDeclaration])) <>
                          (untag $ (getTypeScriptDeclaration :: Tagged OneField [TSDeclaration])) <>
                          (untag $ (getTypeScriptDeclaration :: Tagged TwoFieldRecordless [TSDeclaration])) <>
                          (untag $ (getTypeScriptDeclaration :: Tagged TwoField [TSDeclaration])) <>
                          (untag $ (getTypeScriptDeclaration :: Tagged TwoConstructor [TSDeclaration]))
                         )

      let typesAndValues = [(untag $ (getTypeScriptType :: Tagged Unit String), A.encode Unit)
                           , (untag $ (getTypeScriptType :: Tagged OneFieldRecordless String), A.encode $ OneFieldRecordless 42)
                           , (untag $ (getTypeScriptType :: Tagged OneField String), A.encode $ OneField "asdf")
                           , (untag $ (getTypeScriptType :: Tagged TwoFieldRecordless String), A.encode $ TwoFieldRecordless 42 "asdf")
                           , (untag $ (getTypeScriptType :: Tagged TwoField String), A.encode $ TwoField 42 "asdf")
                           , (untag $ (getTypeScriptType :: Tagged TwoConstructor String), A.encode $ Con1 "asdf")
                           , (untag $ (getTypeScriptType :: Tagged TwoConstructor String), A.encode $ Con2 "asdf" 42)
                           ]

      testTypeCheckDeclarations declarations typesAndValues


main = defaultMainWithIngredients defaultIngredients tests

main' = putStrLn $ formatTSDeclarations (
  unTagged (getTypeScriptDeclaration :: Tagged Unit [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged OneFieldRecordless [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged OneField [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged TwoFieldRecordless [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged TwoField [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged TwoConstructor [TSDeclaration])
  )
