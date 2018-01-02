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

tests = unsafePerformIO $ testSpec "Token tests" $ do

  describe "TaggedObject" $ do
    describe "single constructor with tagSingleConstructors=False" $ do
      it [i|with a single nullary constructor like #{A.encode Unit}|] $ do
        (getTypeScriptType :: Tagged Unit String) `shouldBe` "Unit"
        (getTypeScriptDeclaration :: Tagged Unit [TSDeclaration]) `shouldBe` (Tagged [])

      it [i|with a single non-record constructor like #{A.encode $ OneFieldRecordless 42}|] $ do
        (getTypeScriptType :: Tagged OneFieldRecordless String) `shouldBe` "OneFieldRecordless"
        (getTypeScriptDeclaration :: Tagged OneFieldRecordless [TSDeclaration]) `shouldBe` (Tagged $ [

          ])

      it [i|with a single record constructor like #{A.encode $ OneField "asdf"}|] $ do
        (getTypeScriptType :: Tagged OneField String) `shouldBe` "OneField"
        (getTypeScriptDeclaration :: Tagged OneField [TSDeclaration]) `shouldBe` (Tagged [

          ])

      it [i|with a two-field non-record constructor like #{A.encode $ TwoFieldRecordless 42 "asdf"}|] $ do
        (getTypeScriptType :: Tagged TwoFieldRecordless String) `shouldBe` "TwoFieldRecordless"
        (getTypeScriptDeclaration :: Tagged TwoFieldRecordless [TSDeclaration]) `shouldBe` (Tagged [

          ])

      it [i|with a two-field record constructor like #{A.encode $ TwoField 42 "asdf"}|] $ do
        (getTypeScriptType :: Tagged TwoField String) `shouldBe` "TwoField"
        (getTypeScriptDeclaration :: Tagged TwoField [TSDeclaration]) `shouldBe` (Tagged [

          ])


main = defaultMainWithIngredients defaultIngredients tests
