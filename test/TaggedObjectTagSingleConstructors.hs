
module TaggedObjectTagSingleConstructors (main, tests) where

import Data.Aeson as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "TaggedObject with tagSingleConstructors=True" (setTagSingleConstructors A.defaultOptions))

main :: IO ()
main = hspec tests
