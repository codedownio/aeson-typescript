{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module TwoElemArrayNoTagSingleConstructors (tests) where

import Data.Aeson as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "TwoElemArray with tagSingleConstructors=False" (A.defaultOptions {sumEncoding=TwoElemArray}))

main = hspec tests
