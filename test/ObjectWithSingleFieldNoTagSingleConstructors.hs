{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module ObjectWithSingleFieldNoTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "ObjectWithSingleField with tagSingleConstructors=False" (A.defaultOptions {sumEncoding=ObjectWithSingleField}))

main = hspec tests
