{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module ObjectWithSingleFieldNoTagSingleConstructors (tests) where

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

tests = unsafePerformIO $ testSpec "ObjectWithSingleField with tagSingleConstructors=False" $ do
  it "type checks everything with tsc" $ do
    testTypeCheckDeclarations declarations typesAndValues


main = defaultMainWithIngredients defaultIngredients tests
