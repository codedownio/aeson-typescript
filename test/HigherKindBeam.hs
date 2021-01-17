{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module HigherKindBeam where

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
import Language.Haskell.TH hiding (Type)
import Prelude hiding (Double)


data SingleDE = SingleDE
data K8SDE = K8SDE

data K8SEnvironment = K8SEnvironment
  deriving (Eq, Show)

data SingleNodeEnvironment = SingleNodeEnvironment
  deriving (Eq, Show)

type family DeployEnvironment env = result | result -> env where
  DeployEnvironment SingleNodeEnvironment = SingleDE
  DeployEnvironment K8SEnvironment = K8SDE


data UserT env f = User {
  _userUsername :: Columnar f T.Text
  , _userCreatedAt :: Columnar f UTCTime
  , _userDeployEnvironment :: Columnar f (DeployEnvironment env)
  }

instance TypeScript Identity where
  getTypeScriptType _ = "any"
instance TypeScript UTCTime where
  getTypeScriptType _ = "any"
             


-- $(deriveTypeScript A.defaultOptions ''UserT)


-- instance TypeScript (f_1 :: * -> *) => TypeScript (HigherKindBeam.UserT (f_1 :: * -> *)) where
--   getTypeScriptType _ = mappend "UserT" (mconcat ["<", head [getTypeScriptType (Proxy :: Proxy (f_1 :: * -> *))]
--                                                  , mconcat [mappend ", " x_2 | x_2 <- tail [getTypeScriptType (Proxy :: Proxy (f_1 :: * -> *))]]
--                                                  , ">"])
--   getParentTypes _ = [TSType (Proxy :: Proxy HigherKindBeam.UserT)]



-- main = putStrLn $ formatTSDeclarations (
--    (getTypeScriptDeclarations (Proxy :: Proxy (UserT Identity)))
--   )
