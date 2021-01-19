{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Live2 where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Proxy

       
data TestT a = TestT {
  listOfA :: [a]
  , maybeA :: Maybe a
  }
$(deriveTypeScript A.defaultOptions ''TestT)

main :: IO ()
main = getTypeScriptDeclarations (Proxy :: Proxy (TestT Int))
     & formatTSDeclarations
     & putStrLn
