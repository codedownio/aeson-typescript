
module NoOmitNothingFields (allTests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Test.Hspec
import TestBoilerplate

$(testDeclarations "NoOmitNothingFields" (A.defaultOptions {omitNothingFields = False}))

allTests :: SpecWith ()
allTests = describe "NoOmitNothingFields" $ do
  it "encodes as expected" $ do
    let decls = getTypeScriptDeclarations (Proxy :: Proxy OptionalRecord)

    decls `shouldBe` [TSTypeAlternatives "OptionalRecord" [] ["IOptionalRecord"] Nothing
                     , TSInterfaceDeclaration "IOptionalRecord" [] [TSField False "optionalInt" "number | null" Nothing] Nothing]

  tests
