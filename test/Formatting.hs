{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Formatting (tests) where

import Control.Exception
import Data.Aeson (SumEncoding(UntaggedValue), defaultOptions, sumEncoding, tagSingleConstructors)
import Data.Aeson.TypeScript.TH
import Data.Proxy
import Data.String.Interpolate
import Test.Hspec


data D = S | F deriving (Eq, Show)
$(deriveTypeScript defaultOptions ''D)

data D2 = S2 | F2 deriving (Eq, Show)
$(deriveTypeScript defaultOptions ''D2)

-- A.encode U --> "[]"
data Unit = U deriving (Eq, Show)
$(deriveTypeScript defaultOptions ''Unit)

-- A.encode UTagSingle --> "\"UTagSingle\""
data UnitTagSingle = UTagSingle deriving (Eq, Show)
$(deriveTypeScript (defaultOptions { tagSingleConstructors = True, sumEncoding = UntaggedValue }) ''UnitTagSingle)

data PrimeInType' = PrimeInType
$(deriveTypeScript defaultOptions ''PrimeInType')

data PrimeInConstr = PrimeInConstr'
$(deriveTypeScript defaultOptions ''PrimeInConstr)

data FooBar =
  Foo {
    -- | @no-emit-typescript
    recordString :: String
    , recordInt :: Int
    }
  |
  -- | @no-emit-typescript
  Bar {
      barInt :: Int
  }
$(deriveTypeScript defaultOptions ''FooBar)

data NormalConstructors =
  -- | @no-emit-typescript
  Con1 String
  | Con2 Int
$(deriveTypeScript defaultOptions ''NormalConstructors)

data SimpleEnum =
  EnumA
  | -- | @no-emit-typescript
    EnumB
  | EnumC
$(deriveTypeScript defaultOptions ''SimpleEnum)

tests :: Spec
tests = describe "Formatting" $ do
  describe "when given a Sum Type" $ do
    describe "and the TypeAlias format option is set" $
      it "should generate a TS string literal type" $
        formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @D Proxy) `shouldBe`
          [i|type D = "S" | "F";|]

    describe "and the Enum format option is set" $ do
      it "should generate a TS Enum" $
        formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = Enum }) (getTypeScriptDeclarations @D Proxy) `shouldBe`
          [i|enum D { S="S", F="F" }|]

      it "should generate a TS Enum with multiple" $
        formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = Enum }) (getTypeScriptDeclarations @D Proxy <> getTypeScriptDeclarations @D2 Proxy) `shouldBe`
          [__i|enum D { S="S", F="F" }

               enum D2 { S2="S2", F2="F2" }|]

      it "should generate a normal type from Unit, singe tagSingleConstructors=False by default" $
        formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = Enum }) (getTypeScriptDeclarations @Unit Proxy) `shouldBe`
          [__i|type Unit = IU;

               type IU = void[];|]

      it "should generate a suitable enum from UnitTagSingle" $
        formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = Enum }) (getTypeScriptDeclarations @UnitTagSingle Proxy) `shouldBe`
          [__i|enum UnitTagSingle { UTagSingle="UTagSingle" }|]

    describe "and the EnumWithType format option is set" $ do
      it "should generate a TS Enum with a type declaration" $
        formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = EnumWithType }) (getTypeScriptDeclarations @D Proxy) `shouldBe`
          [i|enum DEnum { S="S", F="F" }\n\ntype D = keyof typeof DEnum;|]

      it "should also work for UnitTagSingle" $
        formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = EnumWithType }) (getTypeScriptDeclarations @UnitTagSingle Proxy) `shouldBe`
          [i|enum UnitTagSingleEnum { UTagSingle="UTagSingle" }\n\ntype UnitTagSingle = keyof typeof UnitTagSingleEnum;|]

  describe "when the name has an apostrophe" $ do
    describe "in the type" $ do
      it "throws an error" $ do
        evaluate (formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @PrimeInType' Proxy)) `shouldThrow` anyErrorCall

    describe "in the constructor" $ do
      it "throws an error" $ do
        evaluate (formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @PrimeInConstr Proxy)) `shouldThrow` anyErrorCall

#if MIN_VERSION_template_haskell(2,18,0)
  describe "when @no-emit-typescript is present" $ do
    it [i|works on records and constructors of record types|] $ do
      formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @FooBar Proxy) `shouldBe` [i|type FooBar = IFoo;\n\ninterface IFoo {\n  tag: "Foo";\n  recordInt: number;\n}|]

    it [i|works on normal constructors|] $ do
      formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @NormalConstructors Proxy) `shouldBe` [i|type NormalConstructors = ICon2;\n\ninterface ICon2 {\n  tag: "Con2";\n  contents: number;\n}|]

    it [i|works on nullary constructors in simple enums|] $ do
      formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @SimpleEnum Proxy) `shouldBe` [i|type SimpleEnum = "EnumA" | "EnumC";|]
#endif

main :: IO ()
main = hspec tests
