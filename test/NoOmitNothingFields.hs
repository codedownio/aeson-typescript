{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module NoOmitNothingFields (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "NoOmitNothingFields" (A.defaultOptions {omitNothingFields=False}))

main = hspec tests
