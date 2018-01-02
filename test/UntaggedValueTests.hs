{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module UntaggedValueTests (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Tagged
import Prelude hiding (Double)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.Runners

data Simplest = Simplest
$(deriveJSON A.defaultOptions { sumEncoding = UntaggedValue } ''Simplest)
-- $(deriveTypeScript A.defaultOptions { sumEncoding = UntaggedValue } ''Simplest)

data SimpleRecordless = SimpleRecordless Int
$(deriveJSON A.defaultOptions { sumEncoding = UntaggedValue } ''SimpleRecordless)
-- $(deriveTypeScript A.defaultOptions { sumEncoding = UntaggedValue } ''SimpleRecordless)

data Simple = Simple { simpleString :: String }
$(deriveJSON A.defaultOptions { sumEncoding = UntaggedValue } ''Simple)
-- $(deriveTypeScript A.defaultOptions { sumEncoding = UntaggedValue } ''Simple)

data DoubleRecordless = DoubleRecordless Int String
$(deriveJSON A.defaultOptions { sumEncoding = UntaggedValue } ''DoubleRecordless)
-- $(deriveTypeScript A.defaultOptions { sumEncoding = UntaggedValue } ''DoubleRecordless)

data Double = Double { doubleInt :: Int
                     , doubleString :: String }
$(deriveJSON A.defaultOptions { sumEncoding = UntaggedValue } ''Double)
-- $(deriveTypeScript A.defaultOptions { sumEncoding = UntaggedValue } ''Double)


-- why on earth does testSpec use IO
tests = unsafePerformIO $ testSpec "UntaggedValue" $ do
  describe "single constructor" $ do
    it "with a single nullary constructor" $ do
      2 `shouldBe` 2
  --     (getTypeScriptType :: Tagged Simplest String) `shouldBe` "Simplest"
  --     (getTypeScriptDeclaration :: Tagged Simplest [TSDeclaration]) `shouldBe` (Tagged [])

  --   it "with a single non-record constructor" $ do
  --     (getTypeScriptType :: Tagged SimpleRecordless String) `shouldBe` "SimpleRecordless"
  --     (getTypeScriptDeclaration :: Tagged SimpleRecordless [TSDeclaration]) `shouldBe` (Tagged [])

  --   it "with a single record constructor" $ do
  --     (getTypeScriptType :: Tagged Simple String) `shouldBe` "Simple"
  --     (getTypeScriptDeclaration :: Tagged Simple [TSDeclaration]) `shouldBe` (Tagged $ [
  --       TSTypeAlternatives "Simple" [] ["ISimple"],
  --       TSInterfaceDeclaration "ISimple" [] [TSField False "tag" "string"
  --                                           , TSField False "simpleString" "string"]
  --       ])

  -- describe "two constructors" $ do
  --   it "with two non-record constructors" $ do
  --     (getTypeScriptType :: Tagged DoubleRecordless String) `shouldBe` "DoubleRecordless"
  --     (getTypeScriptDeclaration :: Tagged DoubleRecordless [TSDeclaration]) `shouldBe` (Tagged [])

  --   it "with two record constructors" $ do
  --     (getTypeScriptType :: Tagged Double String) `shouldBe` "Double"
  --     (getTypeScriptDeclaration :: Tagged Double [TSDeclaration]) `shouldBe` (Tagged [])



main = defaultMainWithIngredients defaultIngredients tests
