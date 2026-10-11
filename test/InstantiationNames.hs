{-# LANGUAGE UndecidableInstances #-}

module InstantiationNames (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Kind (Type)
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


type family Chosen (f :: Type -> Type) x y where
  Chosen Terse x y = x
  Chosen Verbose x y = y

data Terse (x :: Type)
instance TypeScript Terse where
  getTypeScriptType _ = "any"

data Verbose (x :: Type)
instance TypeScript Verbose where
  getTypeScriptType _ = "any"
  getTypeScriptNameSuffix _ = "Verbose"

data Switched f = Switched { switchedThing :: Chosen f Int String }
$(deriveTypeScript' A.defaultOptions ''Switched (defaultExtraTypeScriptOptions { nameSuffixTypeVariables = ["f"] }))

type family FamA env where
  FamA T = ()
type family FamB env where
  FamB T = ()

data Holder env = Holder { holderThing :: Int }
$(deriveTypeScript' A.defaultOptions ''Holder (defaultExtraTypeScriptOptions { extraGenericVariableConstraints = [''FamA, ''FamB] }))


tests :: SpecWith ()
tests = describe "Instantiation-dependent names" $ do
  describe "nameSuffixTypeVariables" $ do
    it [i|leaves the names alone for a tag with no suffix|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy (Switched Terse))) `shouldBe` ([
        TSTypeAlternatives "Switched" [] ["ISwitched"] Nothing
        , TSInterfaceDeclaration "ISwitched" [] [TSField False "switchedThing" "number" Nothing] Nothing
        ])

      (getTypeScriptType (Proxy :: Proxy (Switched Terse))) `shouldBe` "Switched"

    it [i|gives the other instantiation its own names, so the two can't collide|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy (Switched Verbose))) `shouldBe` ([
        TSTypeAlternatives "SwitchedVerbose" [] ["ISwitchedVerbose"] Nothing
        , TSInterfaceDeclaration "ISwitchedVerbose" [] [TSField False "switchedThing" "string" Nothing] Nothing
        ])

      (getTypeScriptType (Proxy :: Proxy (Switched Verbose))) `shouldBe` "SwitchedVerbose"

  describe "extraGenericVariableConstraints" $ do
    it [i|repeats keyof for each one, since "keyof A & B" would parse as "(keyof A) & B"|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy (Holder T))) `shouldBe` ([
        TSTypeAlternatives "Holder" ["T extends keyof FamA & keyof FamB"] ["IHolder<T>"] Nothing
        , TSInterfaceDeclaration "IHolder" ["T extends keyof FamA & keyof FamB"] [
            TSField False "holderThing" "number" Nothing
            ] Nothing
        ])


main :: IO ()
main = hspec tests
