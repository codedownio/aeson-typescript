{-# LANGUAGE CPP #-}

module UntaggedTagSingleConstructors (main, tests) where

import Data.Aeson as A
import Test.Hspec
import TestBoilerplate
import Util

-- Between Aeson 0.11.3.0 and 1.0.0.0, UntaggedValue was added
-- Disable these tests if it's not present
#if MIN_VERSION_aeson(1,0,0)
$(testDeclarations "UntaggedTagSingleConstructors" (setTagSingleConstructors $ A.defaultOptions {sumEncoding=UntaggedValue}))
#else
tests :: SpecWith ()
tests = describe "UntaggedTagSingleConstructors" $ it "tests are disabled for this Aeson version" $ 2 `shouldBe` 2
#endif

main :: IO ()
main = hspec tests
