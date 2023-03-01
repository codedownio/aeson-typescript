
module ObjectWithSingleFieldNoTagSingleConstructors (main, tests) where

import Data.Aeson as A
import Test.Hspec
import TestBoilerplate

$(testDeclarations "ObjectWithSingleField with tagSingleConstructors=False" (A.defaultOptions {sumEncoding=ObjectWithSingleField}))

main :: IO ()
main = hspec tests
