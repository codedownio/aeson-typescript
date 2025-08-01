
module MaybeTuples (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


data Maybe1 = Maybe1 (Maybe Int)
deriveTypeScript A.defaultOptions ''Maybe1

data Maybe2 = Maybe2 String (Maybe Int)
deriveTypeScript A.defaultOptions ''Maybe2

data Maybe3 = Maybe3 String (String, String) (Maybe Int)
deriveTypeScript A.defaultOptions ''Maybe3

data Maybe4 = Maybe4 Int Int Int (Maybe Int)
deriveTypeScript A.defaultOptions ''Maybe4

data Maybe5 = Maybe5 Int Int Int Int (Maybe Int)
deriveTypeScript A.defaultOptions ''Maybe5

data Maybe6 = Maybe6 Int Int Int Int Int (Maybe Int)
deriveTypeScript A.defaultOptions ''Maybe6

data MaybeRecord = MaybeRecord {
  foo :: String
  , bar :: Maybe Int
  }
deriveTypeScript A.defaultOptions ''MaybeRecord

tests :: SpecWith ()
tests = describe "Maybes in tuple encodings" $ do
  describe "tagSingleConstructors and constructorTagModifier" $ do
    it [i|Maybe 1 tuple encoding includes null option|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Maybe1)) `shouldBe` ([
        TSTypeAlternatives "Maybe1" [] ["IMaybe1"] Nothing
        , TSTypeAlternatives "IMaybe1" [] ["number | null"] Nothing
        ])

    it [i|Maybe 2 tuple encoding includes null option|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Maybe2)) `shouldBe` ([
        TSTypeAlternatives "Maybe2" [] ["IMaybe2"] Nothing
        , TSTypeAlternatives "IMaybe2" [] ["[string, number | null]"] Nothing
        ])

    it [i|Maybe 3 tuple encoding includes null option|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Maybe3)) `shouldBe` ([
        TSTypeAlternatives "Maybe3" [] ["IMaybe3"] Nothing
        , TSTypeAlternatives "IMaybe3" [] ["[string, [string, string], number | null]"] Nothing
        ])

    it [i|Maybe 4 tuple encoding includes null option|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Maybe4)) `shouldBe` ([
        TSTypeAlternatives "Maybe4" [] ["IMaybe4"] Nothing
        , TSTypeAlternatives "IMaybe4" [] ["[number, number, number, number | null]"] Nothing
        ])

    it [i|Maybe 5 tuple encoding includes null option|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Maybe5)) `shouldBe` ([
        TSTypeAlternatives "Maybe5" [] ["IMaybe5"] Nothing
        , TSTypeAlternatives "IMaybe5" [] ["[number, number, number, number, number | null]"] Nothing
        ])

    it [i|Maybe 6 tuple encoding includes null option|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Maybe6)) `shouldBe` ([
        TSTypeAlternatives "Maybe6" [] ["IMaybe6"] Nothing
        , TSTypeAlternatives "IMaybe6" [] ["[number, number, number, number, number, number | null]"] Nothing
        ])


main :: IO ()
main = hspec tests
