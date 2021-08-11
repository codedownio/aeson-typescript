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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Generic (tests) where

import Data.Aeson as A
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate
import Prelude hiding (Double)
import Test.Hspec


data Complex a = Product Int a | Unary Int deriving Eq
$(deriveTypeScript defaultOptions ''Complex)

data Complex2 a = Product2 Int a
$(deriveTypeScript (defaultOptions { sumEncoding = UntaggedValue }) ''Complex2)

tests :: SpecWith ()
tests = describe "Generic instances" $ do
  it [i|Complex makes the declaration and types correctly|] $ do
    (getTypeScriptDeclarationsRecursively (Proxy :: Proxy (Complex String))) `shouldBe` [
      TSInterfaceDeclaration {interfaceName = "IProduct", interfaceGenericVariables = ["T"], interfaceMembers = [TSField {fieldOptional = False, fieldName = "tag", fieldType = "\"Product\""},TSField {fieldOptional = False, fieldName = "contents", fieldType = "[number, T]"}]}
      ,TSInterfaceDeclaration {interfaceName = "IUnary", interfaceGenericVariables = ["T"], interfaceMembers = [TSField {fieldOptional = False, fieldName = "tag", fieldType = "\"Unary\""},TSField {fieldOptional = False, fieldName = "contents", fieldType = "number"}]}
      ,TSTypeAlternatives {typeName = "Complex", typeGenericVariables = ["T"], alternativeTypes = ["IProduct<T>","IUnary<T>"]}
      ]

  it [i|Complex2 makes the declaration and types correctly|] $ do
    (getTypeScriptDeclarationsRecursively (Proxy :: Proxy (Complex2 String))) `shouldBe` [
      TSTypeAlternatives {typeName = "Complex2", typeGenericVariables = ["T"], alternativeTypes = ["IProduct2<T>"]}
      ,TSTypeAlternatives {typeName = "IProduct2", typeGenericVariables = ["T"], alternativeTypes = ["[number, T]"]}
      ]

main :: IO ()
main = hspec tests
