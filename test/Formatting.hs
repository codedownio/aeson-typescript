{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Formatting (tests) where

import Data.Aeson (defaultOptions)
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate
import Test.Hspec

data D = S | F deriving (Eq, Show)

$(deriveTypeScript defaultOptions ''D)

tests :: Spec
tests = do
  describe "Formatting" $
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
