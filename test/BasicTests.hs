
module BasicTests where

import Data.Aeson.TypeScript.TH
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.Runners

data Foo = Foo { fooString :: String }

-- data HigherKind a = HigherKind { higherKindList :: [a] }

-- $(deriveTypeScript A.defaultOptions ''HigherKind)

-- data Foo = Foo { fooString :: String
--                , fooInt :: Int }
--          | Bar { barString :: String
--                , barMaybe :: Maybe Int
--                , bazReference :: Baz
--                , higherKindReference :: HigherKind String }

-- data Baz = Baz { bazString :: String }

-- $(deriveTypeScript A.defaultOptions ''Foo)
-- $(deriveTypeScript A.defaultOptions ''Baz)

-- data T = T
-- instance TypeScript T where
--   getTypeScriptType = Tagged "T"
--   getTypeScriptDeclaration = Tagged []

-- main = putStrLn $ formatTSDeclarations (
--   unTagged (getTypeScriptDeclaration :: Tagged (HigherKind T) [TSDeclaration]) <>
--   unTagged (getTypeScriptDeclaration :: Tagged Foo [TSDeclaration]) <>
--   unTagged (getTypeScriptDeclaration :: Tagged Baz [TSDeclaration])
--   )

-- why on earth does testSpec use IO
tests = unsafePerformIO $ testSpec "Token tests" $ do
  describe "basic test" $ do
    it "does a basic thing" $ do
      2 `shouldBe` 2
