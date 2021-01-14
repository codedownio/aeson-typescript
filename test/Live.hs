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
import Language.Haskell.TH
import Prelude hiding (Double)
import Database.Beam



data TestT a = TestT {
  listOfA :: [a]
  , maybeA :: Maybe a
  }

$(deriveTypeScript A.defaultOptions ''TestT)



instance TypeScript F1 where
  getTypeScriptType _ = "any"

instance TypeScript Identity where
  getTypeScriptType _ = "any"

instance TypeScript UTCTime where
  getTypeScriptType _ = "DateTime"
                        
data UserT f = User {
  _userUsername :: Columnar f T.Text
  , _userCreatedAt  :: Columnar f UTCTime
  }


$(deriveTypeScript A.defaultOptions ''UserT)
             
-- main = do
--   putStrLn $(stringE . pprint =<< (deriveTypeScript A.defaultOptions ''TestT))
