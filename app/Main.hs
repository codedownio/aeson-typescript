{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, KindSignatures #-}

module Main where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Data
import Data.Monoid
import Data.String.Interpolate.IsString
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype


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

data T = T
instance TypeScript T where
  getTypeScriptType = Tagged "T"
  getTypeScriptDeclaration = Tagged []

main = putStrLn $ formatTSDeclarations (
  unTagged (getTypeScriptDeclaration :: Tagged (HigherKind T) [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged Foo [TSDeclaration]) <>
  unTagged (getTypeScriptDeclaration :: Tagged Baz [TSDeclaration])
  )
