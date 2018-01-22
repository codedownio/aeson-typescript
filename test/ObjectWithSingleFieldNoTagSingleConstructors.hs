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

tests = unsafePerformIO $ testSpec "ObjectWithSingleField with tagSingleConstructors=False" $ do
  it "generates the right output" $ do
    let file = getTSFile declarations typesAndValues
    file `shouldBe` [i|
type Unit = IUnit;

type IUnit = void[];

type OneFieldRecordless = IOneFieldRecordless;

type IOneFieldRecordless = number;

type OneField = IOneField;

interface IOneField {
  simpleString: string;
}

type TwoFieldRecordless = ITwoFieldRecordless;

type ITwoFieldRecordless = [number, string];

type TwoField = ITwoField;

interface ITwoField {
  doubleInt: number;
  doubleString: string;
}

type TwoConstructor = {\"Con1\": ICon1} | {\"Con2\": ICon2};

interface ICon1 {
  con1String: string;
}

interface ICon2 {
  con2String: string;
  con2Int: number;
}

let x1: Unit = [];
let x2: OneFieldRecordless = 42;
let x3: OneField = {\"simpleString\":\"asdf\"};
let x4: TwoFieldRecordless = [42,\"asdf\"];
let x5: TwoField = {\"doubleInt\":42,\"doubleString\":\"asdf\"};
let x6: TwoConstructor = {\"Con1\":{\"con1String\":\"asdf\"}};
let x7: TwoConstructor = {\"Con2\":{\"con2String\":\"asdf\",\"con2Int\":42}};

|]

  it "type checks everything with tsc" $ do
    testTypeCheckDeclarations declarations typesAndValues


main = defaultMainWithIngredients defaultIngredients tests
