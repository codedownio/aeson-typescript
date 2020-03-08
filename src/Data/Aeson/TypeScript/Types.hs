{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, PolyKinds, StandaloneDeriving #-}

module Data.Aeson.TypeScript.Types where

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Proxy
import Data.String
import Data.Typeable

-- | The typeclass that defines how a type is turned into TypeScript.
--
-- The 'getTypeScriptDeclarations' method describes the top-level declarations that are needed for a type,
-- while 'getTypeScriptType' describes how references to the type should be translated. The 'getTypeScriptOptional'
-- method exists purely so that 'Maybe' types can be encoded with a question mark.
--
--  Instances for common types are built-in and are usually very simple; for example,
--
-- @
-- instance TypeScript Bool where
--   getTypeScriptType _ = "boolean"
-- @
--
-- Most of the time you should not need to write instances by hand; in fact, the 'TSDeclaration'
-- constructors are deliberately opaque. However, you may occasionally need to specify the type of something.
-- For example, since 'UTCTime' is encoded to a JSON string and is not built-in to this library:
--
-- @
-- import Data.Time.Clock (UTCTime)
--
-- instance TypeScript UTCTime where
--   getTypeScriptType _ = "string"
-- @
--
-- If you need to write a definition for a higher-order type, it may depend on a type parameter. For example,
-- a 'Set' is encoded to a JSON list of the underlying type:
--
-- @
-- instance (TypeScript a) => TypeScript (Set a) where
--   getTypeScriptType _ = getTypeScriptType (Proxy :: Proxy a) <> "[]";
-- @
class (Typeable a) => TypeScript a where
  getTypeScriptDeclarations :: Proxy a -> [TSDeclaration]
  -- ^ Get the declaration(s) needed for this type.
  getTypeScriptDeclarations _ = []

  getTypeScriptType :: Proxy a -> String
  -- ^ Get the type as a string.

  getTypeScriptOptional :: Proxy a -> Bool
  -- ^ Get a flag representing whether this type is optional.
  getTypeScriptOptional _ = False

  getParentTypes :: Proxy a -> [TSType]
  getParentTypes _ = []
  -- ^ Get the types that this type depends on. This is useful for generating transitive closures of necessary types.

  getListTypeScriptType :: Proxy [a] -> String
  getListTypeScriptType _ = (getTypeScriptType (Proxy :: Proxy a)) ++ "[]"
  -- ^ Allow the programmer to assign a specialised type to lists. For example,
  -- this is used by the predefined TypeScript instance of the Char type,
  -- where the type String should have "string" assigned to them, rather than "char[]".

  getListParentTypes :: Proxy [a] -> [TSType]
  getListParentTypes _ = (TSType (Proxy :: Proxy a)) : (getParentTypes (Proxy :: Proxy a))
  -- ^ Allow the programmer to specify parent types specialised to lists. For example,
  -- this is used by the predefined TypeScript instance of the Char type,
  -- where the type String should have no parent types
  -- because TypeScript does not know any type for single characters.

-- | An existential wrapper for any TypeScript instance.
data TSType = forall a. (Typeable a, TypeScript a) => TSType { unTSType :: Proxy a }

instance Eq TSType where
  (TSType p1) == (TSType p2) = typeRep p1 == typeRep p2

instance Ord TSType where
  (TSType p1) `compare` (TSType p2) = typeRep p1 `compare` typeRep p2

instance Show TSType where
  show (TSType proxy) = show $ typeRep proxy

data TSDeclaration = TSInterfaceDeclaration { interfaceName :: String
                                            , interfaceGenericVariables :: [String]
                                            , interfaceMembers :: [TSField] }
                   | TSTypeAlternatives { typeName :: String
                                        , typeGenericVariables :: [String]
                                        , alternativeTypes :: [String]}
  deriving (Show, Eq, Ord)

data TSField = TSField { fieldOptional :: Bool
                       , fieldName :: String
                       , fieldType :: String } deriving (Show, Eq, Ord)

newtype TSString a = TSString { unpackTSString :: String } deriving Show

instance IsString (TSString a) where
  fromString = TSString

-- * Formatting options

data FormattingOptions = FormattingOptions
  { numIndentSpaces       :: Int
  -- ^ How many spaces to indent TypeScript blocks
  , interfaceNameModifier :: String -> String
  -- ^ Function applied to generated interface names
  , typeNameModifier :: String -> String
  -- ^ Function applied to generated type names
  }

defaultFormattingOptions = FormattingOptions
  { numIndentSpaces = 2
  , interfaceNameModifier = id
  , typeNameModifier = id
  }

-- | Convenience typeclass class you can use to "attach" a set of Aeson encoding options to a type.
class HasJSONOptions a where
  getJSONOptions :: (Proxy a) -> A.Options
