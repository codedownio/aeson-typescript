{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances #-}

module Data.Aeson.TypeScript.Instances where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Aeson.TypeScript.Types
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

instance TypeScript T.Text where
  getTypeScriptType = Tagged "string"
  getTypeScriptDeclaration = Tagged []

instance TypeScript Integer where
  getTypeScriptType = Tagged "number"
  getTypeScriptDeclaration = Tagged []

instance TypeScript Bool where
  getTypeScriptType = Tagged "boolean"
  getTypeScriptDeclaration = Tagged []

instance TypeScript Int where
  getTypeScriptDeclaration = Tagged []
  getTypeScriptType = Tagged "number"

instance TypeScript Char where
  getTypeScriptDeclaration = Tagged []
  getTypeScriptType = Tagged "string"
  getTypeScriptSpecialInfo = Tagged $ Just IsChar

instance (TypeScript a) => TypeScript [a] where
  getTypeScriptType = Tagged (if specialInfo == (Just IsChar) then "string" else (baseType ++ "[]")) where
    baseType = unTagged (getTypeScriptType :: Tagged a String)
    specialInfo = unTagged (getTypeScriptSpecialInfo :: Tagged a (Maybe SpecialInfo))
  getTypeScriptDeclaration = Tagged []

instance (TypeScript a, TypeScript b) => TypeScript (a, b) where
  getTypeScriptType = Tagged [i|[#{unTagged $ (getTypeScriptType :: Tagged a String)}, #{unTagged $ (getTypeScriptType :: Tagged b String)}]|]
  getTypeScriptDeclaration = Tagged []

instance (TypeScript a) => TypeScript (Maybe a) where
  getTypeScriptDeclaration = Tagged [] :: Tagged (Maybe a) [TSDeclaration]
  getTypeScriptType = Tagged (unTagged $ (getTypeScriptType :: Tagged a String))
  getTypeScriptOptional = Tagged True

instance TypeScript A.Value where
  getTypeScriptDeclaration = Tagged []
  getTypeScriptType = Tagged "any";
