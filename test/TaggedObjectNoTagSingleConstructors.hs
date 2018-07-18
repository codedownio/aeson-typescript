{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module TaggedObjectNoTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Monoid ((<>))
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Prelude hiding (Double)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "TaggedObject with tagSingleConstructors=False" A.defaultOptions)

main = hspec tests
