{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Formatting (tests) where

import Data.Aeson (defaultOptions)
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Test.Hspec

data D = S | F deriving (Eq, Show)

$(deriveTypeScript defaultOptions ''D)

tests :: Spec
tests = do
  let stringTypeLiteralType = "type D = \"S\" | \"F\";"
  let enum = "enum D {   S,   F }"
  let enumWithType = "enum DEnum {   S=\"S\",   F=\"F\" }\n\ntype D = keyof typeof DEnum;"

  describe "Formatting" $
    describe "when given a Sum Type" $ do
      describe "and the TypeAlias format option is set" $
        it "should generate a TS string literal type" $
          formatTSDeclarations' defaultFormattingOptions (getTypeScriptDeclarations @D Proxy) `shouldBe` stringTypeLiteralType
      describe "and the Enum format option is set" $
        it "should generate a TS Enum" $
          formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = Enum }) (getTypeScriptDeclarations @D Proxy) `shouldBe` enum
      describe "and the EnumWithType format option is set" $
        it "should generate a TS Enum with a type declaration" $
          formatTSDeclarations' (defaultFormattingOptions { typeAlternativesFormat = EnumWithType }) (getTypeScriptDeclarations @D Proxy) `shouldBe` enumWithType
