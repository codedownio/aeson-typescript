{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Formatting (tests) where

import Control.Exception
import Data.Aeson (defaultOptions)
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate
import Test.Hspec

data D = S | F deriving (Eq, Show)

$(deriveTypeScript defaultOptions ''D)

data PrimeInType' = PrimeInType

$(deriveTypeScript defaultOptions ''PrimeInType')

data PrimeInConstr = PrimeInConstr'

$(deriveTypeScript defaultOptions ''PrimeInConstr)

tests :: Spec
tests = do
  describe "Formatting" $ do
    describe "when given a Sum Type" $ do
      describe "and the TypeAlias format option is set" $
        it "should generate a TS string literal type" $
          formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @D Proxy) `shouldBe`
            [i|type D = "S" | "F";|]
      describe "and the Enum format option is set" $
        it "should generate a TS Enum" $
          formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = Enum }) (getTypeScriptDeclarations @D Proxy) `shouldBe`
            [i|enum D { S, F }|]
      describe "and the EnumWithType format option is set" $
        it "should generate a TS Enum with a type declaration" $
          formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = EnumWithType }) (getTypeScriptDeclarations @D Proxy) `shouldBe`
            [i|enum DEnum { S="S", F="F" }\n\ntype D = keyof typeof DEnum;|]
    describe "when the name has an apostrophe" $ do
      describe "in the type" $ do
        it "throws an error" $ do
          evaluate (formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @PrimeInType' Proxy))
            `shouldThrow`
              anyErrorCall
      describe "in the constructor" $ do
        it "throws an error" $ do
          evaluate (formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @PrimeInConstr Proxy))
            `shouldThrow`
              anyErrorCall

