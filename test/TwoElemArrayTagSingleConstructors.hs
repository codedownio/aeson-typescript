{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module TwoElemArrayTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "TwoElemArray with tagSingleConstructors=True" (setTagSingleConstructors $ A.defaultOptions {sumEncoding=TwoElemArray}))

main = hspec tests
