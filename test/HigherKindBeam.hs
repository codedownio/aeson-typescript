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

module HigherKindBeam where

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
import Prelude hiding (Double)
-- import Test.Hspec




data UserT f = User {
  _userUsername :: Columnar f T.Text
  , _userCreatedAt  :: Columnar f UTCTime
  }

instance TypeScript Identity where
  getTypeScriptType _ = "any"
instance TypeScript UTCTime where
  getTypeScriptType _ = "any"
             

-- $(deriveTypeScript A.defaultOptions ''UserT)
instance (TypeScript (f_1 :: * -> *), TypeScript (Columnar f_1 T.Text), TypeScript (Columnar f_1 UTCTime)) => TypeScript (HigherKindBeam.UserT (f_1 :: * -> *)) where
  getTypeScriptType _ = mappend "UserT" (mconcat ["<", head [getTypeScriptType (Proxy :: Proxy f_1)]
                                                 , mconcat [mappend ", " x_0 | x_0 <- tail [getTypeScriptType (Proxy :: Proxy f_1)]]
                                                 ,">"])
  getTypeScriptDeclarations _ = [TSTypeAlternatives "UserT" ["T"] ["IUser<T>"]
                                , TSInterfaceDeclaration "IUser" ["T"] [
                                    TSField (getTypeScriptOptional (Proxy :: Proxy (Columnar f_1 T.Text))) "_userUsername" (getTypeScriptType (Proxy :: Proxy (Columnar f_1 T.Text)))
                                    , TSField (getTypeScriptOptional (Proxy :: Proxy (Columnar f_1 UTCTime))) "_userCreatedAt" (getTypeScriptType (Proxy :: Proxy (Columnar f_1 UTCTime)))
                                    ]]
  getParentTypes _ = [TSType (Proxy :: Proxy (Columnar f_1 T.Text)), TSType (Proxy :: Proxy (Columnar f_1 UTCTime))]


-- instance TypeScript (f_1 :: * -> *) => TypeScript (HigherKindBeam.UserT (f_1 :: * -> *)) where
--   getTypeScriptType _ = mappend "UserT" (mconcat ["<", head [getTypeScriptType (Proxy :: Proxy (f_1 :: * -> *))]
--                                                  , mconcat [mappend ", " x_2 | x_2 <- tail [getTypeScriptType (Proxy :: Proxy (f_1 :: * -> *))]]
--                                                  , ">"])
--   getParentTypes _ = [TSType (Proxy :: Proxy HigherKindBeam.UserT)]



-- main = putStrLn $ formatTSDeclarations (
--    (getTypeScriptDeclarations (Proxy :: Proxy (UserT Identity)))
--   )
