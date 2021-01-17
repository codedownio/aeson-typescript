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

module UntaggedTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Test.Hspec
import TestBoilerplate
import Util

-- Between Aeson 0.11.3.0 and 1.0.0.0, UntaggedValue was added
-- Disable these tests if it's not present
#if MIN_VERSION_aeson(1,0,0)
$(testDeclarations "UntaggedTagSingleConstructors" (setTagSingleConstructors $ A.defaultOptions {sumEncoding=UntaggedValue}))
#else
tests = describe "UntaggedTagSingleConstructors" $ it "tests are disabled for this Aeson version" $ 2 `shouldBe` 2
#endif

main = hspec tests
