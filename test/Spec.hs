
module Main where

import Test.Tasty

import BasicTests as BasicTests

allTests :: TestTree
allTests = testGroup "All unit tests" [BasicTests.tests]

main = defaultMainWithIngredients defaultIngredients $ allTests
