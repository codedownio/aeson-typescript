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
import Data.Char (toLower)
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


data TwoField = TwoField { doubleInt :: Int, doubleString :: String }
$(deriveTypeScript' A.defaultOptions ''TwoField defaultExtraTypeScriptOptions {omitFields =["doubleInt"]})

data EmptyTypeScript = EmptyTypeScript { emptyInt :: Int, emptyString :: String }
$(deriveTypeScript' A.defaultOptions ''EmptyTypeScript defaultExtraTypeScriptOptions {omitFields =["emptyInt", "emptyString"]})

data IgnoreFieldLabelModifier = IgnoreFieldLabelModifier { prefixInt :: Int, prefixString :: String }
$(deriveTypeScript' A.defaultOptions {A.fieldLabelModifier = map toLower . drop 6} ''IgnoreFieldLabelModifier defaultExtraTypeScriptOptions {omitFields =["int", "prefixString"]})

allTests :: SpecWith ()
allTests = describe "OmitFields tests" $ do
    it [i|removes a field|] $ do
        (getTypeScriptDeclarations (Proxy :: Proxy TwoField)) `shouldBe` [
            TSTypeAlternatives {typeName = "TwoField", typeGenericVariables = [], alternativeTypes = ["ITwoField"]},
            TSInterfaceDeclaration {interfaceName = "ITwoField", interfaceGenericVariables = [], interfaceMembers = [TSField {fieldOptional = False, fieldName = "doubleString", fieldType = "string"}]}
            ]

    it [i|can remove all fields|] $ do
        (getTypeScriptDeclarations (Proxy :: Proxy EmptyTypeScript)) `shouldBe` [
            TSTypeAlternatives {typeName = "EmptyTypeScript", typeGenericVariables = [], alternativeTypes = ["IEmptyTypeScript"]},
            TSInterfaceDeclaration {interfaceName = "IEmptyTypeScript", interfaceGenericVariables = [], interfaceMembers = []}
            ]

    it [i|ignores aeson field label modifiers|] $ do
        (getTypeScriptDeclarations (Proxy :: Proxy IgnoreFieldLabelModifier)) `shouldBe` [
            TSTypeAlternatives {typeName = "IgnoreFieldLabelModifier", typeGenericVariables = [], alternativeTypes = ["IIgnoreFieldLabelModifier"]},
            TSInterfaceDeclaration {interfaceName = "IIgnoreFieldLabelModifier", interfaceGenericVariables = [], interfaceMembers = [TSField {fieldOptional = False, fieldName = "int", fieldType = "number"}]}
            ]
