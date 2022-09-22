{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Aeson.TypeScript.Types where

import qualified Data.Aeson as A
import Data.Proxy
import Data.String
import Data.Typeable
import Language.Haskell.TH

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

  getTypeScriptKeyType :: Proxy a -> String
  getTypeScriptKeyType proxy = getTypeScriptType proxy
  -- ^ Get the key type as a string.

  getTypeScriptOptional :: Proxy a -> Bool
  -- ^ Get a flag representing whether this type is optional.
  getTypeScriptOptional _ = False

  getParentTypes :: Proxy a -> [TSType]
  -- ^ Get the types that this type depends on. This is useful for generating transitive closures of necessary types.
  getParentTypes _ = []

  isGenericVariable :: Proxy a -> Bool
  -- ^ Special flag to indicate whether this type corresponds to a template variable.
  isGenericVariable _ = False

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
                   | TSRawDeclaration { text :: String }
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
  , exportMode :: ExportMode
  -- ^ Whether to include the export keyword in declarations
  , typeAlternativesFormat :: SumTypeFormat
  -- ^ How to format the declaration of the alternatives when multiple constructors exist
  }

data ExportMode =
  ExportEach
  -- ^ Prefix every declaration with the "export" keyword (suitable for putting in a TypeScripe module)
  | ExportNone
  -- ^ No exporting (suitable for putting in a .d.ts file)

-- | TODO: docstrings here
data SumTypeFormat =
  TypeAlias
  | Enum
  | EnumWithType
  deriving (Eq, Show)

defaultFormattingOptions :: FormattingOptions
defaultFormattingOptions = FormattingOptions
  { numIndentSpaces = 2
  , interfaceNameModifier = id
  , typeNameModifier = id
  , exportMode = ExportNone
  , typeAlternativesFormat = TypeAlias
  }

-- | Convenience typeclass class you can use to "attach" a set of Aeson encoding options to a type.
class HasJSONOptions a where
  getJSONOptions :: (Proxy a) -> A.Options

data T = T
data T1 = T1
data T2 = T2
data T3 = T3
data T4 = T4
data T5 = T5
data T6 = T6
data T7 = T7
data T8 = T8
data T9 = T9
data T10 = T10

instance TypeScript T where getTypeScriptType _ = "T"; isGenericVariable _ = True
instance TypeScript T1 where getTypeScriptType _ = "T1"; isGenericVariable _ = True
instance TypeScript T2 where getTypeScriptType _ = "T2"; isGenericVariable _ = True
instance TypeScript T3 where getTypeScriptType _ = "T3"; isGenericVariable _ = True
instance TypeScript T4 where getTypeScriptType _ = "T4"; isGenericVariable _ = True
instance TypeScript T5 where getTypeScriptType _ = "T5"; isGenericVariable _ = True
instance TypeScript T6 where getTypeScriptType _ = "T6"; isGenericVariable _ = True
instance TypeScript T7 where getTypeScriptType _ = "T7"; isGenericVariable _ = True
instance TypeScript T8 where getTypeScriptType _ = "T8"; isGenericVariable _ = True
instance TypeScript T9 where getTypeScriptType _ = "T9"; isGenericVariable _ = True
instance TypeScript T10 where getTypeScriptType _ = "T10"; isGenericVariable _ = True

allStarConstructors :: [Type]
allStarConstructors = [ConT ''T1, ConT ''T2, ConT ''T3, ConT ''T4, ConT ''T5, ConT ''T6, ConT ''T7, ConT ''T8, ConT ''T9, ConT ''T10]

allStarConstructors' :: [Name]
allStarConstructors' = [''T1, ''T2, ''T3, ''T4, ''T5, ''T6, ''T7, ''T8, ''T9, ''T10]

allStarConstructors'' :: [String]
allStarConstructors'' = ["T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8", "T9", "T10"]

-- | Type variable gathering

data ExtraTypeScriptOptions = ExtraTypeScriptOptions {
  typeFamiliesToMapToTypeScript :: [Name]
  , keyType :: Maybe String
  , omitFields :: [String]
  }

defaultExtraTypeScriptOptions :: ExtraTypeScriptOptions
defaultExtraTypeScriptOptions = ExtraTypeScriptOptions [] Nothing []

data ExtraDeclOrGenericInfo = ExtraDecl Exp
                            | ExtraGeneric GenericInfo
                            | ExtraTopLevelDecs [Dec]
                            | ExtraConstraint Type
                            | ExtraParentType Type
  deriving Show

data GenericInfo = GenericInfo Name GenericInfoExtra
  deriving Show

data GenericInfoExtra = NormalStar
                      | TypeFamilyKey Name
  deriving Show
