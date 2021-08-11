{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DeriveGeneric #-}

module Live5 where

import Data.Aeson as A
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Kind as Kind
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import Data.Void
import GHC.Generics
import TestBoilerplate


-- data From = FromServer | FromClient
-- data MethodType = Notification | Request

-- data Method (f :: From) (t :: MethodType) where
--   Login :: Method 'FromClient 'Request
--   ReportClick :: Method 'FromClient 'Notification

-- instance TypeScript Login where getTypeScriptType _ = "asdf"
-- instance TypeScript ReportClick where getTypeScriptType _ = "fdsa"

-- data LoginParams = LoginParams {
--   loginUsername :: T.Text
--   , loginPassword :: T.Text
--   }
-- $(deriveJSONAndTypeScript A.defaultOptions ''LoginParams)


-- data ReportClickParams = ReportClickParams {
--   reportClickX :: Int
--   , reportClickY :: Int
--   }
-- $(deriveJSONAndTypeScript A.defaultOptions ''ReportClickParams)

-- type family MessageParams (m :: Method f t) :: Kind.Type where
--   MessageParams 'Login = LoginParams
--   MessageParams 'ReportClick = ReportClickParams

-- data SMethod (m :: Method f t) where
--   SLogin :: SMethod 'Login
--   SReportClick :: SMethod 'ReportClick

-- data RequestMessage (m :: Method f 'Request) =
--   RequestMessage {
--     _id :: T.Text
--     , _method :: SMethod m
--     , _params :: MessageParams m
--     }

-- data LoginResult = LoginResult { profilePicture :: T.Text }
-- $(deriveJSONAndTypeScript A.defaultOptions ''LoginResult)

-- type family ResponseResult (m :: Method f 'Request) :: Kind.Type where
--   ResponseResult 'Login = LoginResult
--   ResponseResult _ = Void

-- deriveTypeScript' A.defaultOptions ''RequestMessage (ExtraTypeScriptOptions [''MessageParams])

-- -- main :: IO ()
-- -- main = getTypeScriptDeclarationsRecursively (Proxy @(RequestMessage (Method FromClient Request)))
-- --      & formatTSDeclarations
-- --      & putStrLn
