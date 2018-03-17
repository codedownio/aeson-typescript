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

import Debug.Trace

data HigherKind a = HigherKind { higherKindList :: [a] }

$(deriveTypeScript A.defaultOptions ''HigherKind)

data Foo = Foo { fooString :: String
               , fooInt :: Int }
         | Bar { barString :: String
               , barMaybe :: Maybe Int
               , bazReference :: Baz
               , higherKindReference :: HigherKind String }

data Baz = Baz { bazString :: String }

$(deriveTypeScript A.defaultOptions ''Foo)
$(deriveTypeScript A.defaultOptions ''Baz)


data EvenHigherKind a b = EvenHigherKind { someList :: [b]
                                         , higherKindThing :: HigherKind a }

$(deriveTypeScript A.defaultOptions ''EvenHigherKind)

main = putStrLn $ formatTSDeclarations (
  (getTypeScriptDeclarations (Proxy :: Proxy HigherKind))
  <> (getTypeScriptDeclarations (Proxy :: Proxy Foo))
  <> (getTypeScriptDeclarations (Proxy :: Proxy Baz))
  <> (getTypeScriptDeclarations (Proxy :: Proxy D))
  <> (getTypeScriptDeclarations (Proxy :: Proxy EvenHigherKind))
  )


data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Int
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq

d :: D Int
d = Record { testOne = 3
           , testTwo = True
           , testThree = Product "test" 'A' 123
           }

$(deriveTypeScript (defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower}) ''D)
$(deriveJSON (defaultOptions{fieldLabelModifier = drop 4, constructorTagModifier = map toLower}) ''D)


-- type JupyterInterpreterType = String
-- data InterpreterType = Normal
--                      | Jupyter JupyterInterpreterType deriving (Eq, Ord, Show)

-- $(deriveTypeScript (A.defaultOptions { sumEncoding=UntaggedValue }) ''InterpreterType)
