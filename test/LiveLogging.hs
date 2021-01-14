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

module Live where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
import Data.Kind
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Time
import Database.Beam
import Language.Haskell.TH hiding (Type)
import Prelude hiding (Double)
import Database.Beam



data LoggingSource = SGeneral

data LoggingSourceTagged s where
  General :: LoggingSourceTagged 'SGeneral

type family ParamsFamily (q :: LoggingSource) :: Type where
  ParamsFamily 'SGeneral = String

data HigherKindWithTypeFamily s = TapMessageParams { params :: ParamsFamily s }
-- $(deriveTypeScript A.defaultOptions ''HigherKindWithTypeFamily)

-- main = do
--   putStrLn $(stringE . pprint =<< (deriveTypeScript A.defaultOptions ''TestT))
