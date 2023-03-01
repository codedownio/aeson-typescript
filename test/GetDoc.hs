{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GetDoc (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


-- | OneField is a type with a single field
data OneField = OneField {
  -- | This is a simple string
  simpleString :: String
  }
$(deriveTypeScript A.defaultOptions ''OneField)

tests :: SpecWith ()
tests = describe "getDoc tests" $ do
  it [i|Works with a simple record type|] $ do
    (getTypeScriptDeclarations (Proxy :: Proxy OneField)) `shouldBe` ([
      TSTypeAlternatives "OneField" [] ["IOneField"]
      , TSInterfaceDeclaration "IOneField" [] [
          TSField False "simpleString" "string" (Just " This is a simple string")
          ]
      ])

main :: IO ()
main = hspec tests
