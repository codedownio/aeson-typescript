{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Formatting (tests) where

import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Aeson.TypeScript.Formatting

import Data.Aeson (defaultOptions)
import Data.Proxy
import Test.Hspec

data D = S | F deriving (Eq, Show)

$(deriveTypeScript defaultOptions ''D)

tests :: Spec
tests = do
  let stringTypeLiteralType = "type D = \"S\" | \"F\";"
      enum = "enum D {   S,   F }"
      enumWithType = "enum DEnum {   S=\"S\",   F=\"F\" }\n\ntype D = keyof typeof DEnum;"

  describe "Formatting" $
    context "when given a Sum Type" $ do
      context "and the StringLiteralType format option is set" $
        it "should generate a TS string literal type" $
          formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @D Proxy) `shouldBe` stringTypeLiteralType
      context "and the Enum format option is set" $
        it "should generate a TS Enum" $
          formatTSDeclarations' (defaultFormattingOptions { sumTypeFormat = Enum }) (getTypeScriptDeclarations @D Proxy) `shouldBe` enum
      context "and the EnumWithType format option is set" $
        it "should generate a TS Enum with a type declaration" $
          formatTSDeclarations' (defaultFormattingOptions { sumTypeFormat = EnumWithType }) (getTypeScriptDeclarations @D Proxy) `shouldBe` enumWithType
