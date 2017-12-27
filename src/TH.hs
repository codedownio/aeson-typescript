{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances #-}

module TH where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Data
import Data.Monoid
import Data.String
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

data Foo = Foo { fooString :: String
               , fooInt :: Int }
         | Bar { barString :: String
               , barMaybe :: Maybe Int
               } deriving (Data, Typeable)


newtype TSType a = TSType String deriving Show

data TSDeclaration a = TSInterfaceDeclaration { interfaceName :: String
                                              , interfaceMembers :: [TSField] }
                     | TSTypeAlternatives { alternativeTypes :: [String]}
  deriving Show

-- class IsTSField a where
--   isFieldOptional :: a -> Bool
--   getFieldName :: a -> TSType a
--   getFieldType :: a -> TSType a
-- instance IsTSField (TSField a) where
--   isFieldOptional = fieldOptional
--   getFieldName field = fieldName field
  -- getFieldType = fieldType
-- data TSF = forall a. IsTSField a => TSF a

data TSField = TSField { fieldOptional :: Bool
                       , fieldName :: String
                       , fieldType :: String } deriving Show

data TypeScriptString a = TypeScriptString String deriving Show

instance IsString (TypeScriptString a) where
  fromString x = TypeScriptString x

class TypeScript a where
  -- ^ Get the declaration of this type, if necessary.
  -- When Nothing, no declaration is emitted. Nothing is used for types that are already
  -- known to TypeScript, such as primitive types.
  getTypeScriptDeclaration :: [TSDeclaration a]
  getTypeScriptType :: TypeScriptString a

instance TypeScript Int where
  getTypeScriptDeclaration = []
  getTypeScriptType = TypeScriptString "number"

instance TypeScript [Char] where
  getTypeScriptDeclaration = []
  getTypeScriptType = TypeScriptString "string"

-- | Generates a 'TypeScript' instance declaration for the given data type or
-- data family instance constructor.
deriveTypeScript :: A.Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance
                 -- declaration.
                 -> Q [Dec]
deriveTypeScript opts name = do
  DatatypeInfo {..} <- reifyDatatype name

  let getTypeFn = FunD 'getTypeScriptType [Clause [] (NormalB $ LitE $ StringL $ lastNameComponent $ show datatypeName) []]

  constructorsExp <- liftData datatypeCons
  declarationFnBody <- case A.sumEncoding opts of
    A.TaggedObject tagFieldName contentsFieldName -> do

      let interfaceDeclarations = fmap getConstructorDeclaration datatypeCons
      let typeDeclaration = AppE (ConE 'TSTypeAlternatives) (ListE [LitE $ StringL $ getConstructorName x | x <- fmap constructorName datatypeCons])

      return $ NormalB $ ListE (typeDeclaration : interfaceDeclarations)

    A.UntaggedValue -> error [i|UntaggedValue not implemented|]
    A.ObjectWithSingleField -> error [i|ObjectWithSingleField not implemented|]
    A.TwoElemArray -> error [i|TwoElemArray not implemented|]

  let getDeclarationFn = FunD 'getTypeScriptDeclaration [Clause [] declarationFnBody []]

  return $ [InstanceD Nothing [] (AppT (ConT ''TypeScript) (ConT name)) [getTypeFn, getDeclarationFn]]

lastNameComponent :: String -> String
lastNameComponent x = T.unpack $ last $ T.splitOn "." (T.pack x)

lastNameComponent' :: Name -> String
lastNameComponent' = lastNameComponent . show


-- | Return an expression that evaluates to a TSInterfaceDeclaration
getConstructorDeclaration :: ConstructorInfo -> Exp
getConstructorDeclaration (ConstructorInfo {constructorVariant=(RecordConstructor names), ..}) = AppE (AppE (ConE 'TSInterfaceDeclaration) (LitE $ StringL $ getConstructorName constructorName)) members
  where
    namesAndTypes :: [(String, Type)] = zip (fmap show names) constructorFields
    members = ListE [(AppE (AppE (AppE (ConE 'TSField)
                                   (ConE 'False))
                             (LitE $ StringL $ lastNameComponent $ nameString))
                       (AppE (VarE 'show) (SigE (VarE 'getTypeScriptType) (AppT (VarT ''TypeScriptString) typ))))
                    | (nameString, typ) <- namesAndTypes]

getConstructorName :: Name -> String
getConstructorName x = "I" <> lastNameComponent' x
