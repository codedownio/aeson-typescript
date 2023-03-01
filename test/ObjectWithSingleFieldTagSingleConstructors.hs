
module ObjectWithSingleFieldTagSingleConstructors (main, tests) where

import Data.Aeson as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "ObjectWithSingleField with tagSingleConstructors=True" (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}))

main :: IO ()
main = hspec tests
