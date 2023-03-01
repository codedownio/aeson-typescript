
module Generic (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Map
import Data.Proxy
import Data.String.Interpolate
import Data.Text
import Prelude hiding (Double)
import Test.Hspec


data Complex a = Product Int a | Unary Int
$(deriveTypeScript defaultOptions ''Complex)

data Complex2 a = Product2 Int a
$(deriveTypeScript (defaultOptions { sumEncoding = UntaggedValue }) ''Complex2)

data Complex3 k = Product3 { record3 :: [k] }
$(deriveTypeScript defaultOptions ''Complex3)

data Complex4 k = Product4 { record4 :: Map Text k }
$(deriveTypeScript defaultOptions ''Complex4)

tests :: SpecWith ()
tests = describe "Generic instances" $ do
  it [i|Complex makes the declaration and types correctly|] $ do
    (getTypeScriptDeclarationsRecursively (Proxy :: Proxy (Complex String))) `shouldBe` [
      TSInterfaceDeclaration "IProduct" ["T"] [TSField False "tag" "\"Product\"" Nothing, TSField False "contents" "[number, T]" Nothing] Nothing
      ,TSInterfaceDeclaration "IUnary" ["T"] [TSField False "tag" "\"Unary\"" Nothing, TSField False "contents" "number" Nothing] Nothing
      ,TSTypeAlternatives "Complex" ["T"] ["IProduct<T>","IUnary<T>"] Nothing
      ]

  it [i|Complex2 makes the declaration and types correctly|] $ do
    (getTypeScriptDeclarationsRecursively (Proxy :: Proxy (Complex2 String))) `shouldBe` [
      TSTypeAlternatives "Complex2" ["T"] ["IProduct2<T>"] Nothing
      ,TSTypeAlternatives "IProduct2" ["T"] ["[number, T]"] Nothing
      ]

  it [i|Complex3 makes the declaration and types correctly|] $ do
    (getTypeScriptDeclarationsRecursively (Proxy :: Proxy (Complex3 String))) `shouldBe` [
      TSInterfaceDeclaration "IProduct3" ["T"] [TSField False "record3" "T[]" Nothing] Nothing
      ,TSTypeAlternatives "Complex3" ["T"] ["IProduct3<T>"] Nothing
      ]

    (getTypeScriptDeclarationsRecursively (Proxy :: Proxy (Complex3 Int))) `shouldBe` [
      TSInterfaceDeclaration "IProduct3" ["T"] [TSField False "record3" "T[]" Nothing] Nothing
      ,TSTypeAlternatives "Complex3" ["T"] ["IProduct3<T>"] Nothing
      ]

  it [i|Complex4 makes the declaration and types correctly|] $ do
    (getTypeScriptDeclarationsRecursively (Proxy :: Proxy (Complex4 String))) `shouldBe` [
      TSInterfaceDeclaration "IProduct4" ["T"] [TSField False "record4" "{[k in string]?: T}" Nothing] Nothing
      ,TSTypeAlternatives "Complex4" ["T"] ["IProduct4<T>"] Nothing
      ]

main :: IO ()
main = hspec tests
