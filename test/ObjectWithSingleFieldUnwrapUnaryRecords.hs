{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module ObjectWithSingleFieldUnwrapUnaryRecords (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "ObjectWithSingleField with unwrapUnaryRecords=True" A.defaultOptions {unwrapUnaryRecords = True})

main = hspec tests
