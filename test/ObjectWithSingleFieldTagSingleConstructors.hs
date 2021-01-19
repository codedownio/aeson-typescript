{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ObjectWithSingleFieldTagSingleConstructors (main, tests) where

import Data.Aeson as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "ObjectWithSingleField with tagSingleConstructors=True" (setTagSingleConstructors $ A.defaultOptions {sumEncoding=ObjectWithSingleField}))

main :: IO ()
main = hspec tests
