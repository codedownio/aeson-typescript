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

module TwoElemArrayNoTagSingleConstructors (main, tests) where

import Data.Aeson as A
import Test.Hspec
import TestBoilerplate

$(testDeclarations "TwoElemArray with tagSingleConstructors=False" (A.defaultOptions {sumEncoding=TwoElemArray}))

main :: IO ()
main = hspec tests
