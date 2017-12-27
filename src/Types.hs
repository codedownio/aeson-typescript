{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification #-}

module Types where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import Data.Data
import Data.Monoid
import Data.String
import Data.String.Interpolate.IsString
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

class TypeScript a where
  -- ^ Get the declaration of this type, if necessary.
  -- When Nothing, no declaration is emitted. Nothing is used for types that are already
  -- known to TypeScript, such as primitive types.
  getTypeScriptDeclaration :: Tagged a [TSDeclaration]
  getTypeScriptType :: Tagged a String

data TSDeclaration = TSInterfaceDeclaration { interfaceName :: String
                                            , interfaceMembers :: [TSField] }
                   | TSTypeAlternatives { typeName :: String
                                        , alternativeTypes :: [String]}
  deriving Show

data TSField = TSField { fieldOptional :: Bool
                       , fieldName :: String
                       , fieldType :: String } deriving Show

newtype TSString a = TSString { unpackTSString :: String } deriving Show

instance IsString (TSString a) where
  fromString x = TSString x

-- data AnyTSDeclaration = forall i. AnyTSDeclaration i

-- * Formatting options

data FormattingOptions = FormattingOptions { numIndentSpaces :: Int }

defaultFormattingOptions = FormattingOptions 2
