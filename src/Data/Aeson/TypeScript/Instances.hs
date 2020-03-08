{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances #-}

module Data.Aeson.TypeScript.Instances where

import qualified Data.Aeson as A
import Data.Aeson.TypeScript.Types
import Data.Data
import Data.HashMap.Strict
import qualified Data.List as L
import Data.Monoid
import Data.Set
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

instance TypeScript () where
  getTypeScriptType _ = "void"

instance TypeScript T.Text where
  getTypeScriptType _ = "string"

instance TypeScript TL.Text where
  getTypeScriptType _ = "string"

instance TypeScript Integer where
  getTypeScriptType _ = "number"

instance TypeScript Float where
  getTypeScriptType _ = "number"

instance TypeScript Double where
  getTypeScriptType _ = "number"

instance TypeScript Bool where
  getTypeScriptType _ = "boolean"

instance TypeScript Int where
  getTypeScriptType _ = "number"

instance TypeScript Char where
  getTypeScriptType _ = "string"
  getListTypeScriptType _ = "string"
  getListParentTypes _ = []

instance (TypeScript a) => TypeScript [a] where
  getTypeScriptType _ = getListTypeScriptType (Proxy :: Proxy [a])
  getParentTypes _ = getListParentTypes (Proxy :: Proxy [a])

instance (TypeScript a, TypeScript b) => TypeScript (Either a b) where
  getTypeScriptType _ = [i|Either<#{getTypeScriptType (Proxy :: Proxy a)}, #{getTypeScriptType (Proxy :: Proxy b)}>|]
  getTypeScriptDeclarations _ = [TSTypeAlternatives "Either" ["T1", "T2"] ["Left<T1>", "Right<T2>"]
                               , TSInterfaceDeclaration "Left" ["T"] [TSField False "Left" "T"]
                               , TSInterfaceDeclaration "Right" ["T"] [TSField False "Right" "T"]
                               ]
  getParentTypes _ = L.nub ((TSType (Proxy :: Proxy a))
                            : (TSType (Proxy :: Proxy b))
                            : (getParentTypes (Proxy :: Proxy a))
                            <> (getParentTypes (Proxy :: Proxy b)))

instance (TypeScript a, TypeScript b) => TypeScript (a, b) where
  getTypeScriptType _ = [i|[#{getTypeScriptType (Proxy :: Proxy a)}, #{getTypeScriptType (Proxy :: Proxy b)}]|]
  getParentTypes _ = L.nub ((TSType (Proxy :: Proxy a))
                            : (TSType (Proxy :: Proxy b))
                            : (getParentTypes (Proxy :: Proxy a))
                            <> (getParentTypes (Proxy :: Proxy b)))

instance (TypeScript a, TypeScript b, TypeScript c) => TypeScript (a, b, c) where
  getTypeScriptType _ = [i|[#{getTypeScriptType (Proxy :: Proxy a)}, #{getTypeScriptType (Proxy :: Proxy b)}, #{getTypeScriptType (Proxy :: Proxy c)}]|]
  getParentTypes _ = L.nub ((TSType (Proxy :: Proxy a))
                            : (TSType (Proxy :: Proxy b))
                            : (TSType (Proxy :: Proxy c))
                            : (getParentTypes (Proxy :: Proxy a))
                            <> (getParentTypes (Proxy :: Proxy b))
                            <> (getParentTypes (Proxy :: Proxy c)))

instance (TypeScript a, TypeScript b, TypeScript c, TypeScript d) => TypeScript (a, b, c, d) where
  getTypeScriptType _ = [i|[#{getTypeScriptType (Proxy :: Proxy a)}, #{getTypeScriptType (Proxy :: Proxy b)}, #{getTypeScriptType (Proxy :: Proxy c)}, #{getTypeScriptType (Proxy :: Proxy d)}]|]
  getParentTypes _ = L.nub ((TSType (Proxy :: Proxy a))
                            : (TSType (Proxy :: Proxy b))
                            : (TSType (Proxy :: Proxy c))
                            : (TSType (Proxy :: Proxy d))
                            : (getParentTypes (Proxy :: Proxy a))
                            <> (getParentTypes (Proxy :: Proxy b))
                            <> (getParentTypes (Proxy :: Proxy c))
                            <> (getParentTypes (Proxy :: Proxy d)))

instance (TypeScript a) => TypeScript (Maybe a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy a)
  getTypeScriptOptional _ = True
  getParentTypes _ = [TSType (Proxy :: Proxy a)]

instance TypeScript A.Value where
  getTypeScriptType _ = "any";

instance (TypeScript a, TypeScript b) => TypeScript (HashMap a b) where
  getTypeScriptType _ = [i|{[k: #{getTypeScriptType (Proxy :: Proxy a)}]: #{getTypeScriptType (Proxy :: Proxy b)}}|]
  getParentTypes _ = L.nub ((getParentTypes (Proxy :: Proxy a))
                            <> (getParentTypes (Proxy :: Proxy b)))

instance (TypeScript a) => TypeScript (Set a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy a) <> "[]";
  getParentTypes _ = L.nub (getParentTypes (Proxy :: Proxy a))
