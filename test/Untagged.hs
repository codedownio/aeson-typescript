{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module Untagged (tests) where

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

-- Between Aeson 0.11.3.0 and 1.0.0.0, UntaggedValue was added
-- Disable these tests if it's not present
#if MIN_VERSION_aeson(1,0,0)
data Unit = Unit
$(deriveJSON (A.defaultOptions {sumEncoding=UntaggedValue}) ''Unit)
$(deriveTypeScript (A.defaultOptions {sumEncoding=UntaggedValue}) ''Unit)

data OneFieldRecordless = OneFieldRecordless Int
$(deriveJSON (A.defaultOptions {sumEncoding=UntaggedValue}) ''OneFieldRecordless)
$(deriveTypeScript (A.defaultOptions {sumEncoding=UntaggedValue}) ''OneFieldRecordless)

data OneField = OneField { simpleString :: String }
$(deriveJSON (A.defaultOptions {sumEncoding=UntaggedValue}) ''OneField)
$(deriveTypeScript (A.defaultOptions {sumEncoding=UntaggedValue}) ''OneField)

data TwoFieldRecordless = TwoFieldRecordless Int String
$(deriveJSON (A.defaultOptions {sumEncoding=UntaggedValue}) ''TwoFieldRecordless)
$(deriveTypeScript (A.defaultOptions {sumEncoding=UntaggedValue}) ''TwoFieldRecordless)

data TwoField = TwoField { doubleInt :: Int
                         , doubleString :: String }
$(deriveJSON (A.defaultOptions {sumEncoding=UntaggedValue}) ''TwoField)
$(deriveTypeScript (A.defaultOptions {sumEncoding=UntaggedValue}) ''TwoField)

data TwoConstructor = Con1 { con1String :: String }
                    | Con2 { con2String :: String
                           , con2Int :: Int }
$(deriveJSON (A.defaultOptions {sumEncoding=UntaggedValue}) ''TwoConstructor)
$(deriveTypeScript (A.defaultOptions {sumEncoding=UntaggedValue}) ''TwoConstructor)

data MixedNullary = Normal
                  | Other String deriving (Eq, Ord, Show)
$(deriveJSON (A.defaultOptions { sumEncoding=UntaggedValue }) ''MixedNullary)
$(deriveTypeScript (A.defaultOptions { sumEncoding=UntaggedValue }) ''MixedNullary)

declarations = ((getTypeScriptDeclarations (Proxy :: Proxy Unit)) <>
                 (getTypeScriptDeclarations (Proxy :: Proxy OneFieldRecordless)) <>
                 (getTypeScriptDeclarations (Proxy :: Proxy OneField)) <>
                 (getTypeScriptDeclarations (Proxy :: Proxy TwoFieldRecordless)) <>
                 (getTypeScriptDeclarations (Proxy :: Proxy TwoField)) <>
                 (getTypeScriptDeclarations (Proxy :: Proxy TwoConstructor))
               )

typesAndValues = [(getTypeScriptType (Proxy :: Proxy Unit) , A.encode Unit)
                 , (getTypeScriptType (Proxy :: Proxy OneFieldRecordless) , A.encode $ OneFieldRecordless 42)
                 , (getTypeScriptType (Proxy :: Proxy OneField) , A.encode $ OneField "asdf")
                 , (getTypeScriptType (Proxy :: Proxy TwoFieldRecordless) , A.encode $ TwoFieldRecordless 42 "asdf")
                 , (getTypeScriptType (Proxy :: Proxy TwoField) , A.encode $ TwoField 42 "asdf")
                 , (getTypeScriptType (Proxy :: Proxy TwoConstructor) , A.encode $ Con1 "asdf")
                 , (getTypeScriptType (Proxy :: Proxy TwoConstructor) , A.encode $ Con2 "asdf" 42)
                 ]

tests = unsafePerformIO $ testSpec "UntaggedValue" $ do
  it "type checks everything with tsc" $ do
    testTypeCheckDeclarations declarations typesAndValues
#else
tests = unsafePerformIO $ testSpec "UntaggedValue" $ do
  it "tests are disabled for this Aeson version" $ do
    2 `shouldBe` 2
#endif

main = defaultMainWithIngredients defaultIngredients tests
