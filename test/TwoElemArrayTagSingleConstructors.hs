{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module TwoElemArrayTagSingleConstructors (tests) where

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
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''Unit)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''Unit)

data OneFieldRecordless = OneFieldRecordless Int
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''OneFieldRecordless)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''OneFieldRecordless)

data OneField = OneField { simpleString :: String }
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''OneField)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''OneField)

data TwoFieldRecordless = TwoFieldRecordless Int String
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''TwoFieldRecordless)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''TwoFieldRecordless)

data TwoField = TwoField { doubleInt :: Int
                         , doubleString :: String }
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''TwoField)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''TwoField)

data TwoConstructor = Con1 { con1String :: String }
                    | Con2 { con2String :: String
                           , con2Int :: Int }
$(deriveJSON (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''TwoConstructor)
$(deriveTypeScript (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}) ''TwoConstructor)


declarations = ((getTypeScriptDeclaration (Proxy :: Proxy Unit)) <>
                 (getTypeScriptDeclaration (Proxy :: Proxy OneFieldRecordless)) <>
                 (getTypeScriptDeclaration (Proxy :: Proxy OneField)) <>
                 (getTypeScriptDeclaration (Proxy :: Proxy TwoFieldRecordless)) <>
                 (getTypeScriptDeclaration (Proxy :: Proxy TwoField)) <>
                 (getTypeScriptDeclaration (Proxy :: Proxy TwoConstructor))
               )

typesAndValues = [(getTypeScriptType (Proxy :: Proxy Unit) , A.encode Unit)
                 , (getTypeScriptType (Proxy :: Proxy OneFieldRecordless) , A.encode $ OneFieldRecordless 42)
                 , (getTypeScriptType (Proxy :: Proxy OneField) , A.encode $ OneField "asdf")
                 , (getTypeScriptType (Proxy :: Proxy TwoFieldRecordless) , A.encode $ TwoFieldRecordless 42 "asdf")
                 , (getTypeScriptType (Proxy :: Proxy TwoField) , A.encode $ TwoField 42 "asdf")
                 , (getTypeScriptType (Proxy :: Proxy TwoConstructor) , A.encode $ Con1 "asdf")
                 , (getTypeScriptType (Proxy :: Proxy TwoConstructor) , A.encode $ Con2 "asdf" 42)
                 ]

tests = unsafePerformIO $ testSpec "TwoElemArray with tagSingleConstructors=True" $ do
  it "type checks everything with tsc" $ do
    testTypeCheckDeclarations declarations typesAndValues

main = defaultMainWithIngredients defaultIngredients tests
