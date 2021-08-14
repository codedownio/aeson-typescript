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
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Map
import Data.Proxy
import Data.Text


data Complex a = Product Int a | Unary Int deriving Eq

data Complex2 a = Product2 Int a

data Complex3 k = Complex3 {
  bulkCommandNoArgKeys :: Map Text k
  } deriving (Show)
$(deriveTypeScript defaultOptions ''Complex3)

main :: IO ()
-- main = printThing (Proxy @(BulkCommandNoArg Int))
main = printThing (Proxy @(Complex3 Double))

printThing x = getTypeScriptDeclarations x
             & formatTSDeclarations
             & putStrLn
