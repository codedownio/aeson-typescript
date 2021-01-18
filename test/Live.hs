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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Live where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Kind
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Time
import Database.Beam
import Language.Haskell.TH
import Prelude hiding (Double)
import Database.Beam


data TestT a = TestT {
  listOfA :: [a]
  , maybeA :: Maybe a
  }
$(deriveTypeScript A.defaultOptions ''TestT)

instance TypeScript UTCTime where
  getTypeScriptType _ = "DateTime"

instance (Typeable a) => TypeScript (Identity a) where
  getTypeScriptType x = getTypeScriptType x

instance TypeScript Identity where
  getTypeScriptType _ = "any"

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

type family DeployEnvironment env = result | result -> env where
  DeployEnvironment SingleNodeEnvironment = SingleDE
  DeployEnvironment K8SEnvironment = K8SDE
  DeployEnvironment T = ()
                        
data UserT env f = User {
  _userUsername :: Columnar f T.Text
  , _userCreatedAt  :: Columnar f UTCTime
  , _userDeployEnvironment  :: Columnar f (DeployEnvironment env)
  }

-- $(deriveTypeScriptLookupType ''DeployEnvironment "deployEnvDecl")

-- $(deriveTypeScript' A.defaultOptions ''UserT (ExtraTypeScriptOptions [''DeployEnvironment]))


-- data HigherKind a = HigherKind { higherKindList :: [a] }
-- $(deriveTypeScript A.defaultOptions ''HigherKind)

