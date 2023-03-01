
module TwoElemArrayNoTagSingleConstructors (main, tests) where

import Data.Aeson as A
import Test.Hspec
import TestBoilerplate

$(testDeclarations "TwoElemArray with tagSingleConstructors=False" (A.defaultOptions {sumEncoding=TwoElemArray}))

main :: IO ()
main = hspec tests
