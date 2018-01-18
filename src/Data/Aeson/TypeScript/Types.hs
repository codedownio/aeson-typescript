{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, PolyKinds #-}

module Data.Aeson.TypeScript.Types where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import Data.Data
import Data.Monoid
import Data.Proxy
import Data.String
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

class TypeScript a where
  -- ^ Get the declaration of this type, if necessary.
  -- When Nothing, no declaration is emitted. Nothing is used for types that are already
  -- known to TypeScript, such as primitive types.
  getTypeScriptDeclaration :: Proxy a -> [TSDeclaration]
  getTypeScriptDeclaration _ = []

  -- ^ Get the type as a string
  getTypeScriptType :: Proxy a -> String

  -- ^ Get a flag representing whether this type is optional
  getTypeScriptOptional :: Proxy a -> Bool
  getTypeScriptOptional _ = False

  -- ^ Get any special info about this type, used for ad-hoc instance tweaks
  getTypeScriptSpecialInfo :: Proxy a -> Maybe SpecialInfo
  getTypeScriptSpecialInfo _ = Nothing

data SpecialInfo = IsChar deriving Eq

data TSDeclaration = TSInterfaceDeclaration { interfaceName :: String
                                            , interfaceGenericVariables :: [String]
                                            , interfaceMembers :: [TSField] }
                   | TSTypeAlternatives { typeName :: String
                                        , typeGenericVariables :: [String]
                                        , alternativeTypes :: [String]}
                   | TSTwoElemArray { typeName :: String
                                    , typeGenericVariables :: [String]
                                    , alternativeTypes :: [String]}
                   | TSObjectWithSingleField { typeName :: String
                                             , typeGenericVariables :: [String]
                                             , namesAndInterfaces :: [(String, String)]}
  deriving (Show, Eq)

data TSField = TSField { fieldOptional :: Bool
                       , fieldName :: String
                       , fieldType :: String } deriving (Show, Eq)

newtype TSString a = TSString { unpackTSString :: String } deriving Show

instance IsString (TSString a) where
  fromString x = TSString x

-- data AnyTSDeclaration = forall i. AnyTSDeclaration i

-- * Formatting options

data FormattingOptions = FormattingOptions { numIndentSpaces :: Int }

defaultFormattingOptions = FormattingOptions 2


-- | Convenience typeclass class you can use to "attach" a set of Aeson encoding options to a type
-- This way, you can do
-- $(deriveJSON (getJSONOptions (Proxy :: Proxy SomeType)) ''SomeType)
-- $(deriveTypeScript (getJSONOptions (Proxy :: Proxy SomeType)) ''SomeType)
-- and ensure that the options are always in sync
class HasJSONOptions a where
  getJSONOptions :: (Proxy a) -> A.Options
