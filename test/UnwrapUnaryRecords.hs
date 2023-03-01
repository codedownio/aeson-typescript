{-# LANGUAGE CPP #-}

module UnwrapUnaryRecords (allTests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Test.Hspec
import TestBoilerplate


#if MIN_VERSION_aeson(0,10,0)
$(testDeclarations "UnwrapUnaryRecords" (A.defaultOptions {unwrapUnaryRecords = True}))

allTests :: SpecWith ()
allTests = describe "UnwrapUnaryRecords" $ do
  it "encodes as expected" $ do
    let decls = getTypeScriptDeclarations (Proxy :: Proxy OneField)

    decls `shouldBe` [
      TSTypeAlternatives "OneField" [] ["IOneField"] Nothing
      ,TSTypeAlternatives "IOneField" [] ["string"] Nothing
      ]

  tests
#else
tests :: SpecWith ()
tests = describe "UnwrapUnaryRecords" $ it "tests are disabled for this Aeson version" $ 2 `shouldBe` 2

allTests = tests
#endif

main :: IO ()
main = hspec allTests
