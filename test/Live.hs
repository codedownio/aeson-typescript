{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Live where

import Data.Aeson as A
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Prelude hiding (Double)


instance TypeScript Identity where getTypeScriptType _ = "any"

data SingleDE = SingleDE
instance TypeScript SingleDE where getTypeScriptType _ = [i|"single"|]

data K8SDE = K8SDE
instance TypeScript K8SDE where getTypeScriptType _ = [i|"k8s"|]

data SingleNodeEnvironment = SingleNodeEnvironment
  deriving (Eq, Show)
instance TypeScript SingleNodeEnvironment where getTypeScriptType _ = [i|"single_node_env"|]
                                  
data K8SEnvironment = K8SEnvironment
  deriving (Eq, Show)
instance TypeScript K8SEnvironment where getTypeScriptType _ = [i|"k8s_env"|]

data Nullable (c :: Type -> Type) x
data Exposed x
type family Columnar (f :: Type -> Type) x where
    Columnar Exposed x = Exposed x
    Columnar Identity x = x
    Columnar (Nullable c) x = Columnar c (Maybe x)
    Columnar f x = f x

type family DeployEnvironment env = result | result -> env where
  DeployEnvironment SingleNodeEnvironment = SingleDE
  DeployEnvironment K8SEnvironment = K8SDE
  DeployEnvironment T = ()

-- * The main type

data UserT env f = User {
  _userUsername :: Columnar f T.Text
  , _userCreatedAt  :: Columnar f Int
  , _userDeployEnvironment  :: Columnar f (DeployEnvironment env)
  }

$(deriveTypeScript' A.defaultOptions ''UserT (ExtraTypeScriptOptions [''DeployEnvironment]))

main :: IO ()
main = getTypeScriptDeclarationsRecursively (Proxy :: Proxy (UserT T Identity))
     & formatTSDeclarations
     & putStrLn
