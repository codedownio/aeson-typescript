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
import Util


data Unit = Unit
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''Unit)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''Unit)

data OneFieldRecordless = OneFieldRecordless Int
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''OneFieldRecordless)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''OneFieldRecordless)

data OneField = OneField { simpleString :: String }
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''OneField)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''OneField)

data TwoFieldRecordless = TwoFieldRecordless Int String
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoFieldRecordless)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoFieldRecordless)

data TwoField = TwoField { doubleInt :: Int
                         , doubleString :: String }
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoField)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoField)

data TwoConstructor = Con1 { con1String :: String }
                    | Con2 { con2String :: String
                           , con2Int :: Int }
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoConstructor)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}) ''TwoConstructor)


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


tests = describe "ObjectWithSingleField with tagSingleConstructors=True" $ do
  it "type checks everything with tsc" $ do
    testTypeCheckDeclarations declarations typesAndValues

main = hspec tests
