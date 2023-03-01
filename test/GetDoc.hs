
module GetDoc (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


-- | OneField type doc
data OneField =
  -- | OneField constructor doc
  OneField {
    -- | This is a simple string
    simpleString :: String
    }
$(deriveTypeScript A.defaultOptions ''OneField)

tests :: SpecWith ()
tests = describe "getDoc tests" $ do
  it [i|Works with a simple record type|] $ do
    (getTypeScriptDeclarations (Proxy :: Proxy OneField)) `shouldBe` ([
      TSTypeAlternatives "OneField" [] ["IOneField"] (Just "OneField type doc")
      , TSInterfaceDeclaration "IOneField" [] [
          TSField False "simpleString" "string" (Just "This is a simple string")
          ] (Just "OneField constructor doc")
      ])

main :: IO ()
main = hspec tests
