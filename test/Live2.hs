{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Live2 where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Kind
import Data.Function
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Time
import Database.Beam
import Language.Haskell.TH
import Prelude hiding (Double)
import Database.Beam

       
data TestT a = TestT {
  listOfA :: [a]
  , maybeA :: Maybe a
  }
$(deriveTypeScript A.defaultOptions ''TestT)

data HigherKind a = HigherKind { higherKindList :: [a] }
$(deriveTypeScript A.defaultOptions ''HigherKind)

