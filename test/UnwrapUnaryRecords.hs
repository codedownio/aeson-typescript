{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnwrapUnaryRecords (allTests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Proxy
import Test.Hspec
import TestBoilerplate
import Util

-- Between Aeson 0.11.3.0 and 1.0.0.0, UntaggedValue was added
-- Disable these tests if it's not present
#if MIN_VERSION_aeson(0,10,0)
$(testDeclarations "UnwrapUnaryRecords" (setTagSingleConstructors $ A.defaultOptions {unwrapUnaryRecords = True}))

allTests = describe "NoOmitNothingFields" $ do
  it "encodes as expected" $ do
    let decls = getTypeScriptDeclarations (Proxy :: Proxy OneField)

    decls `shouldBe` []

  tests


#else
tests :: SpecWith ()
tests = describe "UnwrapUnaryRecords" $ it "tests are disabled for this Aeson version" $ 2 `shouldBe` 2

allTests = tests
#endif

main :: IO ()
main = hspec allTests
