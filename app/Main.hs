{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import Data.Data
import Data.Monoid
import Data.String.Interpolate.IsString
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Formatting
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import TH


data Foo = Foo { fooString :: String
               , fooInt :: Int }
         | Bar { barString :: String
               , barMaybe :: Maybe Int
               , bazReference :: Baz }

data Baz = Baz { bazString :: String }

$(deriveTypeScript A.defaultOptions ''Foo)
$(deriveTypeScript A.defaultOptions ''Baz)

main = putStrLn $ formatTSDeclarations
  (unTagged (getTypeScriptDeclaration :: Tagged Foo [TSDeclaration]) <>
   unTagged (getTypeScriptDeclaration :: Tagged Baz [TSDeclaration]))
