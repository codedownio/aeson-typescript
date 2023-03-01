{-# LANGUAGE CPP #-}

module Util.Aeson where

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

aesonFromList :: [(K.Key, v)] -> KM.KeyMap v
aesonFromList = KM.fromList
#else
import Data.HashMap.Strict as HM

aesonFromList = HM.fromList
#endif
