
module HigherKind (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec
import Util


data HigherKind a = HigherKind { higherKindList :: [a] }
$(deriveTypeScript A.defaultOptions ''HigherKind)
$(deriveJSON A.defaultOptions ''HigherKind)

data Foo = Foo { fooString :: String
               , fooHigherKindReference :: HigherKind String }
$(deriveTypeScript A.defaultOptions ''Foo)

data DoubleHigherKind a b = DoubleHigherKind { someList :: [b]
                                             , higherKindThing :: HigherKind a }
$(deriveTypeScript A.defaultOptions ''DoubleHigherKind)
$(deriveJSON A.defaultOptions ''DoubleHigherKind)

data HigherKindWithUnary a = Unary Int
$(deriveTypeScript A.defaultOptions ''HigherKindWithUnary)
$(deriveJSON A.defaultOptions ''HigherKindWithUnary)


tests :: SpecWith ()
tests = describe "Higher kinds" $ do
  describe "Kind * -> *" $ do
    it [i|makes the declaration and types correctly|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy (HigherKind T))) `shouldBe` ([
        TSTypeAlternatives "HigherKind" ["T"] ["IHigherKind<T>"] Nothing,
        TSInterfaceDeclaration "IHigherKind" ["T"] [TSField False "higherKindList" "T[]" Nothing] Nothing
        ])

      (getTypeScriptType (Proxy :: Proxy (HigherKind Int))) `shouldBe` "HigherKind<number>"
      (getTypeScriptType (Proxy :: Proxy (HigherKind String))) `shouldBe` "HigherKind<string>"

    it [i|works when referenced in another type|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Foo)) `shouldBe` ([
        TSTypeAlternatives "Foo" [] ["IFoo"] Nothing,
        TSInterfaceDeclaration "IFoo" [] [TSField False "fooString" "string" Nothing
                                         , TSField False "fooHigherKindReference" "HigherKind<string>" Nothing] Nothing
        ])

    it [i|works with an interface inside|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy (HigherKindWithUnary T))) `shouldBe` ([
        TSTypeAlternatives "HigherKindWithUnary" ["T"] ["IUnary<T>"] Nothing,
        TSTypeAlternatives "IUnary" ["T"] ["number"] Nothing
        ])

  describe "Kind * -> * -> *" $ do
    it [i|makes the declaration and type correctly|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy (DoubleHigherKind T1 T2))) `shouldBe` ([
        TSTypeAlternatives "DoubleHigherKind" ["T1","T2"] ["IDoubleHigherKind<T1, T2>"] Nothing,
        TSInterfaceDeclaration "IDoubleHigherKind" ["T1","T2"] [TSField False "someList" "T2[]" Nothing
                                                               , TSField False "higherKindThing" "HigherKind<T1>" Nothing] Nothing
        ])

      (getTypeScriptType (Proxy :: Proxy (DoubleHigherKind Int String))) `shouldBe` "DoubleHigherKind<number, string>"
      (getTypeScriptType (Proxy :: Proxy (DoubleHigherKind String Int))) `shouldBe` "DoubleHigherKind<string, number>"

  describe "TSC compiler checks" $ do
    it "type checks everything with tsc" $ do
      let declarations = ((getTypeScriptDeclarations (Proxy :: Proxy (HigherKind T))) <>
                          (getTypeScriptDeclarations (Proxy :: Proxy (DoubleHigherKind T1 T2))) <>
                          (getTypeScriptDeclarations (Proxy :: Proxy (HigherKindWithUnary T)))
                         )

      let typesAndValues = [(getTypeScriptType (Proxy :: Proxy (HigherKind Int)) , A.encode (HigherKind [42 :: Int]))
                           , (getTypeScriptType (Proxy :: Proxy (HigherKind String)) , A.encode (HigherKind ["asdf" :: String]))

                           , (getTypeScriptType (Proxy :: Proxy (DoubleHigherKind String Int)) , A.encode (DoubleHigherKind [42 :: Int] (HigherKind ["asdf" :: String])))
                           , (getTypeScriptType (Proxy :: Proxy (DoubleHigherKind Int String)) , A.encode (DoubleHigherKind ["asdf" :: String] (HigherKind [42 :: Int])))

                           , (getTypeScriptType (Proxy :: Proxy (HigherKindWithUnary String)) , A.encode ((Unary 42) :: HigherKindWithUnary String))
                           ]

      testTypeCheckDeclarations declarations typesAndValues


main :: IO ()
main = hspec tests

main' :: IO ()
main' = putStrLn $ formatTSDeclarations (
   (getTypeScriptDeclarations (Proxy :: Proxy (HigherKind T))) <>
   (getTypeScriptDeclarations (Proxy :: Proxy (DoubleHigherKind T1 T2))) <>
   (getTypeScriptDeclarations (Proxy :: Proxy (HigherKindWithUnary T)))
  )
