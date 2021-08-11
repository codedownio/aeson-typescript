{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DeriveGeneric #-}

module Live6 where

import Data.Aeson as A
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Proxy


data BulkCommandNoArg k = BulkCommandNoArg {
  bulkCommandNoArgKeys :: [k]
  } deriving (Show)
$(deriveJSONAndTypeScript defaultOptions ''BulkCommandNoArg)


main :: IO ()
main = getTypeScriptDeclarations (Proxy @(BulkCommandNoArg Int))
     & formatTSDeclarations
     & putStrLn
