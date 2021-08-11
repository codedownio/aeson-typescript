{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Live4 where

import Data.Aeson as A
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Proxy
import TestBoilerplate


type family DeployEnvironment2 env = result | result -> env
type instance DeployEnvironment2 SingleNodeEnvironment = SingleDE
type instance DeployEnvironment2 K8SEnvironment = K8SDE
type instance DeployEnvironment2 T = ()
newtype Simple env = Simple (DeployEnvironment2 env)
$(deriveTypeScript' A.defaultOptions ''Simple (ExtraTypeScriptOptions [''DeployEnvironment2]))

main :: IO ()
main = getTypeScriptDeclarationsRecursively (Proxy @(Simple T))
     & formatTSDeclarations
     & putStrLn
