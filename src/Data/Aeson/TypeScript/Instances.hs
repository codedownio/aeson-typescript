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
import Data.Set
import Data.String
import Data.String.Interpolate.IsString
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

instance TypeScript () where
  getTypeScriptType = Tagged "void"

instance TypeScript T.Text where
  getTypeScriptType = Tagged "string"

instance TypeScript TL.Text where
  getTypeScriptType = Tagged "string"

instance TypeScript Integer where
  getTypeScriptType = Tagged "number"

instance TypeScript Bool where
  getTypeScriptType = Tagged "boolean"

instance TypeScript Int where
  getTypeScriptDeclaration = Tagged []
  getTypeScriptType = Tagged "number"

instance TypeScript Char where
  getTypeScriptType = Tagged "string"
  getTypeScriptSpecialInfo = Tagged $ Just IsChar

instance (TypeScript a) => TypeScript [a] where
  getTypeScriptType = Tagged (if specialInfo == (Just IsChar) then "string" else (baseType ++ "[]")) where
    baseType = unTagged (getTypeScriptType :: Tagged a String)
    specialInfo = unTagged (getTypeScriptSpecialInfo :: Tagged a (Maybe SpecialInfo))

instance (TypeScript a, TypeScript b) => TypeScript (Either a b) where
  getTypeScriptType = Tagged [i|Either<#{unTagged $ (getTypeScriptType :: Tagged a String)}, #{unTagged $ (getTypeScriptType :: Tagged b String)}>|]
  getTypeScriptDeclaration = Tagged [TSTypeAlternatives "Either" ["T1", "T2"] ["Left<T1>", "Right<T2>"]
                                    , TSInterfaceDeclaration "Left" ["T"] [TSField False "Left" "T"]
                                    , TSInterfaceDeclaration "Right" ["T"] [TSField False "Right" "T"]
                                    ]

instance (TypeScript a, TypeScript b) => TypeScript (a, b) where
  getTypeScriptType = Tagged [i|[#{unTagged $ (getTypeScriptType :: Tagged a String)}, #{unTagged $ (getTypeScriptType :: Tagged b String)}]|]

instance (TypeScript a, TypeScript b, TypeScript c) => TypeScript (a, b, c) where
  getTypeScriptType = Tagged [i|[#{unTagged $ (getTypeScriptType :: Tagged a String)}, #{unTagged $ (getTypeScriptType :: Tagged b String)}, #{unTagged $ (getTypeScriptType :: Tagged c String)}]|]

instance (TypeScript a) => TypeScript (Maybe a) where
  getTypeScriptType = Tagged (unTagged $ (getTypeScriptType :: Tagged a String))
  getTypeScriptOptional = Tagged True

instance TypeScript A.Value where
  getTypeScriptType = Tagged "any";

instance (TypeScript a) => TypeScript (Set a) where
  getTypeScriptType = Tagged ((unTagged $ (getTypeScriptType :: Tagged a String)) <> "[]");
