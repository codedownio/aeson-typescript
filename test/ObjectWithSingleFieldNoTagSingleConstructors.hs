{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module ObjectWithSingleFieldNoTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import Prelude hiding (Double)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "ObjectWithSingleField with tagSingleConstructors=False" (A.defaultOptions {sumEncoding=ObjectWithSingleField}))

main = hspec tests
