
module Basic (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


data Unit1 = Unit1
$(deriveTypeScript A.defaultOptions ''Unit1)

data Unit2 = Unit2
$(deriveTypeScript (A.defaultOptions { A.tagSingleConstructors = True
                                     , A.constructorTagModifier = const "foo" }) ''Unit2)

data Test1 = Test1 (Maybe Int)
deriveTypeScript A.defaultOptions ''Test1

data Test2 = Test2 String [Int] (Maybe String)
deriveTypeScript A.defaultOptions ''Test2

-- Test case for Maybe types in multi-field tuples
data PromptKey = PromptKey String deriving (Eq, Show)

$(deriveTypeScript A.defaultOptions ''PromptKey)

newtype ExtraInputPrompt = ExtraInputPrompt String deriving (Eq, Show)

$(deriveTypeScript A.defaultOptions ''ExtraInputPrompt)

data WidgetActionPayload
  = InfoRequest String [PromptKey] (Maybe (NonEmpty ExtraInputPrompt))
  deriving (Eq, Show)

$(deriveTypeScript A.defaultOptions ''WidgetActionPayload)

tests :: SpecWith ()
tests = describe "Basic tests" $ do
  describe "tagSingleConstructors and constructorTagModifier" $ do
    it [i|Works with a normal unit|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Unit1)) `shouldBe` ([
        TSTypeAlternatives "Unit1" [] ["IUnit1"] Nothing
        , TSTypeAlternatives "IUnit1" [] ["void[]"] Nothing
        ])

    it [i|Works with a unit with constructorTagModifier|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Unit2)) `shouldBe` ([
        TSTypeAlternatives "Unit2" [] ["\"foo\""] Nothing
        ])

    it [i|Maybe tuple encoding includes null option|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Test1)) `shouldBe` ([
        TSTypeAlternatives "Test1" [] ["ITest1"] Nothing
        , TSTypeAlternatives "ITest1" [] ["number | null"] Nothing
        ])

    it [i|Maybe in multi-field tuple includes null option|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Test2)) `shouldBe` ([
        TSTypeAlternatives "Test2" [] ["ITest2"] Nothing
        , TSTypeAlternatives "ITest2" [] ["[string, number[], string | null]"] Nothing
        ])

    it [i|WidgetActionPayload tuple includes null for Maybe list|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy WidgetActionPayload))
        `shouldBe` ( [ TSTypeAlternatives "WidgetActionPayload" [] ["IInfoRequest"] Nothing,
                       TSTypeAlternatives "IInfoRequest" [] ["[string, PromptKey[], ExtraInputPrompt[] | null]"] Nothing
                     ]
                   )

main :: IO ()
main = hspec tests
