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

module Basic (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import Prelude hiding (Double)
import Test.Hspec
import Util


data Unit1 = Unit1
$(deriveTypeScript A.defaultOptions ''Unit1)

data Unit2 = Unit2
$(deriveTypeScript (A.defaultOptions { A.tagSingleConstructors = True
                                     , A.constructorTagModifier = const "foo" }) ''Unit2)  

tests :: SpecWith ()
tests = describe "Basic tests" $ do
  describe "tagSingleConstructors and constructorTagModifier" $ do
    it [i|Works with a normal unit|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Unit1)) `shouldBe` ([
        TSTypeAlternatives "Unit1" [] ["IUnit1"]
        , TSTypeAlternatives "IUnit1" [] ["void[]"]
        ])

    it [i|Works with a unit with constructorTagModifier|] $ do
      (getTypeScriptDeclarations (Proxy :: Proxy Unit2)) `shouldBe` ([])
       

main :: IO ()
main = hspec tests
