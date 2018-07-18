{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module TaggedObjectTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Monoid ((<>))
import Data.Proxy
import Data.String.Interpolate.IsString
import Prelude hiding (Double)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Util

data Unit = Unit
$(deriveJSON (setTagSingleConstructors A.defaultOptions) ''Unit)
$(deriveTypeScript (setTagSingleConstructors A.defaultOptions) ''Unit)

data OneFieldRecordless = OneFieldRecordless Int
$(deriveJSON (setTagSingleConstructors A.defaultOptions) ''OneFieldRecordless)
$(deriveTypeScript (setTagSingleConstructors A.defaultOptions) ''OneFieldRecordless)

data OneField = OneField { simpleString :: String }
$(deriveJSON (setTagSingleConstructors A.defaultOptions) ''OneField)
$(deriveTypeScript (setTagSingleConstructors A.defaultOptions) ''OneField)

data TwoFieldRecordless = TwoFieldRecordless Int String
$(deriveJSON (setTagSingleConstructors A.defaultOptions) ''TwoFieldRecordless)
$(deriveTypeScript (setTagSingleConstructors A.defaultOptions) ''TwoFieldRecordless)

data TwoField = TwoField { doubleInt :: Int
                         , doubleString :: String }
$(deriveJSON (setTagSingleConstructors A.defaultOptions) ''TwoField)
$(deriveTypeScript (setTagSingleConstructors A.defaultOptions) ''TwoField)

data Hybrid = HybridSimple Int | HybridRecord { hybridString :: String }
$(deriveJSON (setTagSingleConstructors A.defaultOptions) ''Hybrid)
$(deriveTypeScript (setTagSingleConstructors A.defaultOptions) ''Hybrid)

data TwoConstructor = Con1 { con1String :: String }
                    | Con2 { con2String :: String
                           , con2Int :: Int }
$(deriveJSON (setTagSingleConstructors A.defaultOptions) ''TwoConstructor)
$(deriveTypeScript (setTagSingleConstructors A.defaultOptions) ''TwoConstructor)

data Complex a = Nullary
               | Unary Int
               | Product String Char a
               | Record { testOne   :: Int
                        , testTwo   :: Bool
                        , testThree :: Complex a
                        } deriving Eq
$(deriveJSON (setTagSingleConstructors A.defaultOptions) ''Complex)
$(deriveTypeScript (setTagSingleConstructors A.defaultOptions) ''Complex)

declarations = (getTypeScriptDeclarations (Proxy :: Proxy Unit))
            <> (getTypeScriptDeclarations (Proxy :: Proxy OneFieldRecordless))
            <> (getTypeScriptDeclarations (Proxy :: Proxy OneField))
            <> (getTypeScriptDeclarations (Proxy :: Proxy TwoFieldRecordless))
            <> (getTypeScriptDeclarations (Proxy :: Proxy TwoField))
            <> (getTypeScriptDeclarations (Proxy :: Proxy Hybrid))
            <> (getTypeScriptDeclarations (Proxy :: Proxy TwoConstructor))
            <> (getTypeScriptDeclarations (Proxy :: Proxy Complex))

typesAndValues = [(getTypeScriptType (Proxy :: Proxy Unit) , A.encode Unit)

                 , (getTypeScriptType (Proxy :: Proxy OneFieldRecordless) , A.encode $ OneFieldRecordless 42)

                 , (getTypeScriptType (Proxy :: Proxy OneField) , A.encode $ OneField "asdf")

                 , (getTypeScriptType (Proxy :: Proxy TwoFieldRecordless) , A.encode $ TwoFieldRecordless 42 "asdf")

                 , (getTypeScriptType (Proxy :: Proxy TwoField) , A.encode $ TwoField 42 "asdf")

                 , (getTypeScriptType (Proxy :: Proxy TwoConstructor) , A.encode $ Con1 "asdf")
                 , (getTypeScriptType (Proxy :: Proxy TwoConstructor) , A.encode $ Con2 "asdf" 42)

                 , (getTypeScriptType (Proxy :: Proxy Hybrid) , A.encode $ HybridSimple 42)
                 , (getTypeScriptType (Proxy :: Proxy Hybrid) , A.encode $ HybridRecord "asdf")

                 , (getTypeScriptType (Proxy :: Proxy (Complex Int)) , A.encode (Nullary :: Complex Int))
                 , (getTypeScriptType (Proxy :: Proxy (Complex Int)) , A.encode (Unary 42 :: Complex Int))
                 , (getTypeScriptType (Proxy :: Proxy (Complex Int)) , A.encode (Product "asdf" 'g' 42 :: Complex Int))
                 , (getTypeScriptType (Proxy :: Proxy (Complex Int)) , A.encode ((Record { testOne = 3, testTwo = True, testThree = Product "test" 'A' 123}) :: Complex Int))
                 ]

tests = describe "TaggedObject with tagSingleConstructors=True" $ do
  it "type checks everything with tsc" $ do
    testTypeCheckDeclarations declarations typesAndValues

main = hspec tests
