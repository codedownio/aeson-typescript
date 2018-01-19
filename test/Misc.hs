{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, KindSignatures #-}

module Misc where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Prelude hiding (Double)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec (testSpec)
import Test.Tasty.Runners

import Debug.Trace

data HigherKind a = HigherKind { higherKindList :: [a] }

type TypeSyn = Misc.HigherKind Data.Aeson.TypeScript.TH.T1


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
  (getTypeScriptDeclaration (Proxy :: Proxy HigherKind)) <>
  (getTypeScriptDeclaration (Proxy :: Proxy Foo)) <>
  (getTypeScriptDeclaration (Proxy :: Proxy Baz)) <>
  (getTypeScriptDeclaration (Proxy :: Proxy EvenHigherKind))
  )
