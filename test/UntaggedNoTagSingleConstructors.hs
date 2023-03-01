{-# LANGUAGE CPP #-}

module UntaggedNoTagSingleConstructors (main, tests) where

import Data.Aeson as A
import Test.Hspec
import TestBoilerplate

-- Between Aeson 0.11.3.0 and 1.0.0.0, UntaggedValue was added
-- Disable these tests if it's not present
#if MIN_VERSION_aeson(1,0,0)
$(testDeclarations "UntaggedNoTagSingleConstructors" (A.defaultOptions {sumEncoding=UntaggedValue}))
#else
tests = describe "UntaggedNoTagSingleConstructors" $ it "tests are disabled for this Aeson version" $ 2 `shouldBe` 2
#endif

main :: IO ()
main = hspec tests
