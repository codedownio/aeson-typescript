{-# LANGUAGE CPP #-}

module Util.Aeson where

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

aesonFromList :: [(K.Key, v)] -> KM.KeyMap v
aesonFromList = KM.fromList

type AesonKey = K.Key
#else
import Data.Aeson as A
import Data.HashMap.Strict as HM
import Data.Text as T

aesonFromList :: [(T.Text, Value)] -> HM.HashMap Text A.Value
aesonFromList = HM.fromList

type AesonKey = Text
#endif
