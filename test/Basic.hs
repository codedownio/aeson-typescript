
module Basic (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


data Unit1 = Unit1
$(deriveTypeScript A.defaultOptions ''Unit1)

data Unit2 = Unit2
$(deriveTypeScript (A.defaultOptions { A.tagSingleConstructors = True
                                     , A.constructorTagModifier = const "foo" }) ''Unit2)

tests :: SpecWith ()
tests = describe "Basic tests" $ do
  describe "tagSingleConstructors and constructorTagModifier" $ do
    it [i|Works with a normal unit|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Unit1)) `shouldBe` ([
        TSTypeAlternatives "Unit1" [] ["IUnit1"] Nothing
        , TSTypeAlternatives "IUnit1" [] ["void[]"] Nothing
        ])

main :: IO ()
main = hspec tests
