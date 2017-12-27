{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances #-}

module Instances where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Data
import Data.Monoid
import Data.Proxy
import Data.String
import Data.String.Interpolate.IsString
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Types

instance TypeScript Int where
  getTypeScriptDeclaration = Tagged []
  getTypeScriptType = Tagged "number"

instance TypeScript [Char] where
  getTypeScriptDeclaration = Tagged []
  getTypeScriptType = Tagged "number"

instance (TypeScript a) => TypeScript (Maybe a) where
  getTypeScriptDeclaration = Tagged [] :: Tagged (Maybe a) [TSDeclaration]
  getTypeScriptType = Tagged (unTagged $ (getTypeScriptType :: Tagged a String))
