{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module OmitNothingFields (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "OmitNothingFields" (A.defaultOptions {omitNothingFields=True}))

main = hspec tests
