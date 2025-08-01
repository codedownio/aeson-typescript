{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Note: the OverlappingInstances pragma is only here so the overlapping instances in this file
-- will work on older GHCs, like GHC 7.8.4

module Data.Aeson.TypeScript.Instances where

import qualified Data.Aeson as A
import Data.Aeson.TypeScript.Instances.TupleGen
import Data.Aeson.TypeScript.Types
import Data.Data
import Data.Functor.Compose (Compose)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.Functor.Product (Product)
import Data.HashMap.Strict
import Data.HashSet
import Data.Kind (Type)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict
import Data.Set
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Void
import Data.Word
import GHC.Int
import Numeric.Natural (Natural)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as A
#endif


instance TypeScript () where
  getTypeScriptType _ = "void"

instance TypeScript Void where
  getTypeScriptType _ = "void"

instance TypeScript T.Text where
  getTypeScriptType _ = "string"

instance TypeScript TL.Text where
  getTypeScriptType _ = "string"

instance TypeScript Integer where
  getTypeScriptType _ = "number"

instance TypeScript Natural where
  getTypeScriptType _ = "number"

instance TypeScript Float where
  getTypeScriptType _ = "number"

instance TypeScript Double where
  getTypeScriptType _ = "number"

instance TypeScript Bool where
  getTypeScriptType _ = "boolean"

instance TypeScript Int where
  getTypeScriptType _ = "number"

instance TypeScript Int16 where
  getTypeScriptType _ = "number"

instance TypeScript Int32 where
  getTypeScriptType _ = "number"

instance TypeScript Int64 where
  getTypeScriptType _ = "number"

instance TypeScript Char where
  getTypeScriptType _ = "string"

instance TypeScript Word where
  getTypeScriptType _ = "number"

instance TypeScript Word8 where
  getTypeScriptType _ = "number"

instance TypeScript Word16 where
  getTypeScriptType _ = "number"

instance TypeScript Word32 where
  getTypeScriptType _ = "number"

instance TypeScript Word64 where
  getTypeScriptType _ = "number"

instance {-# OVERLAPPABLE #-} (TypeScript a) => TypeScript [a] where
  getTypeScriptType _ = (getTypeScriptType (Proxy :: Proxy a)) ++ "[]"
  getParentTypes _ = [TSType (Proxy :: Proxy a)]

instance (TypeScript a) => TypeScript (NonEmpty a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy [a])
  getParentTypes _ = [TSType (Proxy :: Proxy a)]

instance {-# OVERLAPPING #-} TypeScript [Char] where
  getTypeScriptType _ = "string"

instance (TypeScript a, TypeScript b) => TypeScript (Either a b) where
  getTypeScriptType _ = [i|Either<#{getTypeScriptType (Proxy :: Proxy a)}, #{getTypeScriptType (Proxy :: Proxy b)}>|]
  getTypeScriptDeclarations _ = [TSTypeAlternatives "Either" ["T1", "T2"] ["Left<T1>", "Right<T2>"] Nothing
                               , TSInterfaceDeclaration "Left" ["T"] [TSField False "Left" "T" Nothing] Nothing
                               , TSInterfaceDeclaration "Right" ["T"] [TSField False "Right" "T" Nothing] Nothing
                               ]
  getParentTypes _ = L.nub [ (TSType (Proxy :: Proxy a))
                           , (TSType (Proxy :: Proxy b))
                           ]

-- Derive instance TypeScript (a, b), instance TypeScript (a, b, c), etc. up to size 10
mkTupleInstances 10

instance forall a k (b :: k). (Typeable k, Typeable b, TypeScript a) => TypeScript (Const a b) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy a)
  getParentTypes _ = [TSType (Proxy :: Proxy a)]

instance (TypeScript a) => TypeScript (Identity a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy a)
  getParentTypes _ = [TSType (Proxy :: Proxy a)]

instance forall k k1 (f :: k -> Type) (g :: k1 -> k) a. (
  Typeable k, Typeable k1, Typeable f, Typeable g, Typeable a, TypeScript (f (g a)), TypeScript a
  ) => TypeScript (Compose f g a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy (f (g a)))
  getParentTypes _ = getParentTypes (Proxy :: Proxy (f (g a)))

instance forall k (f :: k -> Type) (g :: k -> Type) a. (
  Typeable k, Typeable f, Typeable g, Typeable a, TypeScript (f a), TypeScript (g a)
  ) => TypeScript (Product f g a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy (f a, g a))
  getParentTypes _ = L.nub [ (TSType (Proxy :: Proxy (f a)))
                           , (TSType (Proxy :: Proxy (g a)))
                           ]

instance (TypeScript a) => TypeScript (Maybe a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy a)
  getTypeScriptOptional _ = True
  getParentTypes _ = [TSType (Proxy :: Proxy a)]

instance TypeScript A.Value where
  getTypeScriptType _ = "any";

instance (TypeScript a, TypeScript b, A.ToJSONKey a) => TypeScript (Map a b) where
  getTypeScriptType =
    let k = getTypeScriptKeyType @a Proxy
        v = getTypeScriptType @b Proxy
    in const $ case A.toJSONKey @a of
      A.ToJSONKeyText {} -> "{[k in " <> k <> "]: " <> v <> "}"
      A.ToJSONKeyValue {} -> getTypeScriptType @[(a, b)] Proxy
  getParentTypes = const $ L.nub [TSType @a Proxy, TSType @b Proxy]

instance (TypeScript a, TypeScript b, A.ToJSONKey a) => TypeScript (HashMap a b) where
  getTypeScriptType = const $ getTypeScriptType @(Map a b) Proxy
  getParentTypes = const $ getParentTypes @(Map a b) Proxy

#if MIN_VERSION_aeson(2,0,0)
instance (TypeScript a) => TypeScript (A.KeyMap a) where
  getTypeScriptType _ = [i|{[k: string]: #{getTypeScriptType (Proxy :: Proxy a)}}|]
  getParentTypes _ = L.nub [TSType (Proxy :: Proxy a)]
#endif

instance (TypeScript a) => TypeScript (Set a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy a) <> "[]";
  getParentTypes _ = [TSType (Proxy :: Proxy a)]

instance (TypeScript a) => TypeScript (HashSet a) where
  getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy a) ++ "[]"
  getParentTypes _ = [TSType (Proxy :: Proxy a)]
