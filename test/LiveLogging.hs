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

module LiveLogging where

import Data.Kind
import Prelude hiding (Double)


data LoggingSource = SGeneral

data LoggingSourceTagged s where
  General :: LoggingSourceTagged 'SGeneral

type family ParamsFamily (q :: LoggingSource) :: Type where
  ParamsFamily 'SGeneral = String

data HigherKindWithTypeFamily s = TapMessageParams { params :: ParamsFamily s }
-- $(deriveTypeScript A.defaultOptions ''HigherKindWithTypeFamily)

-- main = do
--   putStrLn $(stringE . pprint =<< (deriveTypeScript A.defaultOptions ''TestT))
