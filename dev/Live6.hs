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


data Complex a = Product Int a | Unary Int deriving Eq

data Complex2 a = Product2 Int a

data BulkCommandNoArg k = BulkCommandNoArg {
  bulkCommandNoArgKeys :: [k]
  } deriving (Show)
$(deriveTypeScript defaultOptions ''BulkCommandNoArg)

main :: IO ()
-- main = printThing (Proxy @(BulkCommandNoArg Int))
main = printThing (Proxy @(BulkCommandNoArg String))

printThing x = getTypeScriptDeclarations x
             & formatTSDeclarations
             & putStrLn
