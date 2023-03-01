{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module GetDoc (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


data OneField = OneField {
  -- | This is a simple string.
  simpleString :: String
  }
$(deriveTypeScript A.defaultOptions ''OneField)

tests :: SpecWith ()
tests = describe "getDoc tests" $ do
  it [i|Works with a simple record type|] $ do
    (getTypeScriptDeclarations (Proxy :: Proxy OneField)) `shouldBe` ([])


main :: IO ()
main = hspec tests
