{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module Misc where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char
import Data.Monoid ((<>))
import Data.Proxy
import Data.String.Interpolate.IsString
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Prelude hiding (Double)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Util

-- data HigherKind a = HigherKind { higherKindList :: [a] }

-- $(deriveTypeScript A.defaultOptions ''HigherKind)
-- $(deriveJSON A.defaultOptions ''HigherKind)

-- data Foo = Foo { fooString :: String
--                , fooInt :: Int }
--          | Bar { barString :: String
--                , barMaybe :: Maybe Int
--                , bazReference :: Baz
--                , higherKindReference :: HigherKind String }

-- data Baz = Baz { bazString :: String }

-- $(deriveTypeScript A.defaultOptions ''Foo)
-- $(deriveTypeScript A.defaultOptions ''Baz)
-- $(deriveJSON A.defaultOptions ''Foo)
-- $(deriveJSON A.defaultOptions ''Baz)


-- data EvenHigherKind a b = EvenHigherKind { someList :: [b]
--                                          , higherKindThing :: HigherKind a }

-- $(deriveTypeScript A.defaultOptions ''EvenHigherKind)
-- $(deriveJSON A.defaultOptions ''EvenHigherKind)

-- declarations = (getTypeScriptDeclarations (Proxy :: Proxy HigherKind))
--             <> (getTypeScriptDeclarations (Proxy :: Proxy Foo))
--             <> (getTypeScriptDeclarations (Proxy :: Proxy Baz))
--             <> (getTypeScriptDeclarations (Proxy :: Proxy D))
--             <> (getTypeScriptDeclarations (Proxy :: Proxy EvenHigherKind))

-- typesAndValues = [ (getTypeScriptType (Proxy :: Proxy (HigherKind String)) , A.encode (HigherKind ["asdf","ghj"] :: HigherKind String))
--                  , (getTypeScriptType (Proxy :: Proxy Foo) , A.encode (Foo "asdf" 42))
--                  , (getTypeScriptType (Proxy :: Proxy Foo) , A.encode (Bar "asdf" Nothing (Baz "ghj") (HigherKind ["asdf","ghj"])))
--                  , (getTypeScriptType (Proxy :: Proxy Foo) , A.encode (Bar "asdf" (Just 42) (Baz "ghj") (HigherKind ["asdf","ghj"])))
--                  , (getTypeScriptType (Proxy :: Proxy Baz) , A.encode (Baz "asdf"))
--                  , (getTypeScriptType (Proxy :: Proxy (EvenHigherKind Int String)) , A.encode (EvenHigherKind ["asdf","ghi"] (HigherKind [1,2,3]) :: EvenHigherKind Int String))
--                  , (getTypeScriptType (Proxy :: Proxy (D Int)) , A.encode (Nullary :: D Int))
--                  , (getTypeScriptType (Proxy :: Proxy (D Int)) , A.encode (Unary 42 :: D Int))
--                  , (getTypeScriptType (Proxy :: Proxy (D Int)) , A.encode (Product "asdf" 'g' 42 :: D Int))
--                  , (getTypeScriptType (Proxy :: Proxy (D Int)) , A.encode d)
--                  ]

-- tests = describe "Misc" $ do
--   it "type checks everything with tsc" $ do
--     testTypeCheckDeclarations declarations typesAndValues

-- main2 = do
--   putStrLn [i|#{defaultOptions}|]
--   hspec tests


data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Int
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq

$(deriveTypeScript (defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower}) ''D)
$(deriveJSON (defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower}) ''D)


-- type JupyterInterpreterType = String
-- data InterpreterType = Normal
--                      | Jupyter JupyterInterpreterType deriving (Eq, Ord, Show)

-- $(deriveTypeScript (A.defaultOptions { sumEncoding=UntaggedValue }) ''InterpreterType)


main = do
  print defaultOptions
  putStrLn $ formatTSDeclarations (getTypeScriptDeclarations (Proxy :: Proxy D))
  BL8.putStrLn $ A.encode (Nullary :: D Int)
  BL8.putStrLn $ A.encode (Unary 42 :: D Int)
  BL8.putStrLn $ A.encode (Product "asdf" 'g' 42 :: D Int)
  BL8.putStrLn $ A.encode ((Record { testOne = 3, testTwo = True, testThree = Product "test" 'A' 123}) :: D Int)
