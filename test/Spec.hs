
module Main where

import Test.Tasty

import TaggedObjectNoTagSingleConstructors as TaggedObjectNoTagSingleConstructors
import TaggedObjectTagSingleConstructors as TaggedObjectTagSingleConstructors

allTests :: TestTree
allTests = testGroup "All unit tests" [TaggedObjectTagSingleConstructors.tests
                                      , TaggedObjectNoTagSingleConstructors.tests]

main = defaultMainWithIngredients defaultIngredients $ allTests
