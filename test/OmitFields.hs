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

module OmitFields (allTests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


data TwoField = TwoField { doubleInt :: Int, doubleString :: String }
$(deriveTypeScript' A.defaultOptions ''TwoField defaultExtraTypeScriptOptions {omitFields =["doubleInt"]})


allTests :: SpecWith ()
allTests = describe "OmitFields tests" $ do
    it [i|Removes a field|] $ do
        (getTypeScriptDeclarations (Proxy :: Proxy TwoField)) `shouldBe` [
            TSTypeAlternatives {typeName = "TwoField", typeGenericVariables = [], alternativeTypes = ["ITwoField"]},
            TSInterfaceDeclaration {interfaceName = "ITwoField", interfaceGenericVariables = [], interfaceMembers = [TSField {fieldOptional = False, fieldName = "doubleString", fieldType = "string"}]}
            ]

