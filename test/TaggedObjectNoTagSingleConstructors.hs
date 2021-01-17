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

module TaggedObjectNoTagSingleConstructors (tests) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Test.Hspec
import TestBoilerplate
import Util

$(testDeclarations "TaggedObject with tagSingleConstructors=False" A.defaultOptions)

main = hspec tests
