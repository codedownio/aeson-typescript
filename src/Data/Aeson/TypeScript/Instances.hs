{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances #-}

module Data.Aeson.TypeScript.Instances where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Aeson.TypeScript.Types
import Data.Data
import Data.HashMap.Strict
import Data.Monoid
import Data.Proxy
import Data.Set
import Data.String
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

instance TypeScript () where
  getTypeScriptType _ = "void"

instance TypeScript T.Text where
  getTypeScriptType _ = "string"

instance TypeScript TL.Text where
  getTypeScriptType _ = "string"

instance TypeScript Integer where
  getTypeScriptType _ = "number"

instance TypeScript Bool where
  getTypeScriptType _ = "boolean"

instance TypeScript Int where
  getTypeScriptDeclaration _ = []
  getTypeScriptType _ = "number"

instance TypeScript Char where
  getTypeScriptType _ = "string"
  getTypeScriptSpecialInfo _ = Just IsChar

instance (TypeScript a) => TypeScript [a] where
  getTypeScriptType _ = (if specialInfo == (Just IsChar) then "string" else (baseType ++ "[]")) where
    baseType = getTypeScriptType (Proxy :: Proxy a)
    specialInfo = getTypeScriptSpecialInfo (Proxy :: Proxy a)

instance (TypeScript a, TypeScript b) => TypeScript (Either a b) where
  getTypeScriptType _ = [i|Either<#{getTypeScriptType (Proxy :: Proxy a)}, #{getTypeScriptType (Proxy :: Proxy b)}>|]
  getTypeScriptDeclaration _ = [TSTypeAlternatives "Either" ["T1", "T2"] ["Left<T1>", "Right<T2>"]
                               , TSInterfaceDeclaration "Left" ["T"] [TSField False "Left" "T"]
                               , TSInterfaceDeclaration "Right" ["T"] [TSField False "Right" "T"]
                               ]

instance (TypeScript a, TypeScript b) => TypeScript (a, b) where
  getTypeScriptType _ = [i|[#{getTypeScriptType (Proxy :: Proxy a)}, #{getTypeScriptType (Proxy :: Proxy b)}]|]

instance (TypeScript a, TypeScript b, TypeScript c) => TypeScript (a, b, c) where
  getTypeScriptType _ = [i|[#{getTypeScriptType (Proxy :: Proxy a)}, #{getTypeScriptType (Proxy :: Proxy b)}, #{getTypeScriptType (Proxy :: Proxy c)}]|]

instance (TypeScript a, TypeScript b, TypeScript c, TypeScript d) => TypeScript (a, b, c, d) where
  getTypeScriptType _ = [i|[#{getTypeScriptType (Proxy :: Proxy a)}, #{getTypeScriptType (Proxy :: Proxy b)}, #{getTypeScriptType (Proxy :: Proxy c)}, #{getTypeScriptType (Proxy :: Proxy d)}]|]

instance (TypeScript a) => TypeScript (Maybe a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy a)
  getTypeScriptOptional _ = True

instance TypeScript A.Value where
  getTypeScriptType _ = "any";

instance (TypeScript a, TypeScript b) => TypeScript (HashMap a b) where
  getTypeScriptType _ = [i|{[k: #{getTypeScriptType (Proxy :: Proxy a)}]: #{getTypeScriptType (Proxy :: Proxy b)}}|]

instance (TypeScript a) => TypeScript (Set a) where
  getTypeScriptType _ = (((getTypeScriptType (Proxy :: Proxy a))) <> "[]");
