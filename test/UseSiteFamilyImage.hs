{-# LANGUAGE UndecidableInstances #-}

module UseSiteFamilyImage (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Kind (Type)
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


-- A mode-indexed family: the result is one of the family's own arguments, so it has no image
-- on its own. The use site fixes those arguments, and then it does.
type family Chosen (f :: Type -> Type) x y where
  Chosen Terse x y = x
  Chosen Verbose x y = y

data Terse (x :: Type)
instance TypeScript Terse where getTypeScriptType _ = "\"Terse\""

data Verbose (x :: Type)
instance TypeScript Verbose where getTypeScriptType _ = "\"Verbose\""

data Switched f = Switched {
  switchedThing :: Chosen f Int Bool
  , switchedPlain :: Bool
  }
$(deriveTypeScript' A.defaultOptions ''Switched (defaultExtraTypeScriptOptions { typeFamiliesToMapToTypeScript = [''Chosen] }))

-- Two use sites of the same family in one record, so the tables need distinct names.
data TwoSited f = TwoSited {
  twoSitedFirst :: Chosen f Int Bool
  , twoSitedSecond :: Chosen f Char Int
  , twoSitedThird :: Chosen f Char Int
  }
$(deriveTypeScript' A.defaultOptions ''TwoSited (defaultExtraTypeScriptOptions { typeFamiliesToMapToTypeScript = [''Chosen] }))


tests :: SpecWith ()
tests = describe "Use-site type family image" $ do
  it [i|makes one generic declaration, indexed on the mode|] $ do
    (getTypeScriptDeclarations (Proxy :: Proxy (Switched Terse))) `shouldBe` ([
      TSTypeAlternatives "Switched" ["T extends keyof ChosenIntBool"] ["ISwitched<T>"] Nothing
      , TSInterfaceDeclaration "ISwitched" ["T extends keyof ChosenIntBool"] [
          TSField False "switchedThing" "ChosenIntBool[T]" Nothing
          , TSField False "switchedPlain" "boolean" Nothing
          ] Nothing
      ])

  it [i|tabulates the family against the arguments the use site fixed|] $ do
    (getTypeScriptDeclarationsRecursively (Proxy :: Proxy (Switched Terse))) `shouldBe` ([
      TSInterfaceDeclaration "ChosenIntBool" [] [
          TSField False "\"Terse\"" "number" Nothing
          , TSField False "\"Verbose\"" "boolean" Nothing
          ] Nothing
      , TSInterfaceDeclaration "ISwitched" ["T extends keyof ChosenIntBool"] [
          TSField False "switchedThing" "ChosenIntBool[T]" Nothing
          , TSField False "switchedPlain" "boolean" Nothing
          ] Nothing
      , TSTypeAlternatives "Switched" ["T extends keyof ChosenIntBool"] ["ISwitched<T>"] Nothing
      ])

  it [i|renders each instantiation as the matching key, from the one declaration|] $ do
    (getTypeScriptType (Proxy :: Proxy (Switched Terse))) `shouldBe` "Switched<\"Terse\">"
    (getTypeScriptType (Proxy :: Proxy (Switched Verbose))) `shouldBe` "Switched<\"Verbose\">"

  it [i|gives each use site of one family its own table|] $ do
    (getTypeScriptDeclarations (Proxy :: Proxy (TwoSited Terse))) `shouldBe` ([
      TSTypeAlternatives "TwoSited" ["T extends keyof ChosenIntBool & keyof ChosenCharInt"] ["ITwoSited<T>"] Nothing
      , TSInterfaceDeclaration "ITwoSited" ["T extends keyof ChosenIntBool & keyof ChosenCharInt"] [
          TSField False "twoSitedFirst" "ChosenIntBool[T]" Nothing
          , TSField False "twoSitedSecond" "ChosenCharInt[T]" Nothing
          , TSField False "twoSitedThird" "ChosenCharInt[T]" Nothing
          ] Nothing
      ])


main :: IO ()
main = hspec tests
