{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances #-}

module TH (
  module Instances,
  module Types,
  TSDeclaration(..),
  TSField(..),
  deriveTypeScript
  ) where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Data
import Data.Monoid
import Data.String
import Data.String.Interpolate.IsString
import Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Instances
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Types

data Foo = Foo { fooString :: String
               , fooInt :: Int }
         | Bar { barString :: String
               , barMaybe :: Maybe Int
               } deriving (Data, Typeable)

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

  let getTypeFn = FunD 'getTypeScriptType [Clause [] (NormalB $ LitE $ StringL $ getTypeName datatypeName) []]

  constructorsExp <- liftData datatypeCons
  declarationFnBody <- case A.sumEncoding opts of
    A.TaggedObject tagFieldName contentsFieldName -> do

      let interfaceDeclarations = fmap getConstructorDeclaration datatypeCons
      let typeDeclaration = AppE (AppE (ConE 'TSTypeAlternatives) (LitE $ StringL $ getTypeName datatypeName)) (ListE [LitE $ StringL $ getConstructorName x | x <- fmap constructorName datatypeCons])

      return $ NormalB $ AppE (ConE 'Tagged) (ListE (typeDeclaration : interfaceDeclarations))

    A.UntaggedValue -> error [i|UntaggedValue not implemented|]
    A.ObjectWithSingleField -> error [i|ObjectWithSingleField not implemented|]
    A.TwoElemArray -> error [i|TwoElemArray not implemented|]

  let getDeclarationFn = FunD 'getTypeScriptDeclaration [Clause [] declarationFnBody []]

  return $ [InstanceD Nothing [] (AppT (ConT ''TypeScript) (ConT name)) [getTypeFn, getDeclarationFn]]

lastNameComponent :: String -> String
lastNameComponent x = T.unpack $ last $ T.splitOn "." (T.pack x)

lastNameComponent' :: Name -> String
lastNameComponent' = lastNameComponent . show

type TSDeclarations = [TSDeclaration]

-- | Return an expression that evaluates to a TSInterfaceDeclaration
getConstructorDeclaration :: ConstructorInfo -> Exp
getConstructorDeclaration (ConstructorInfo {constructorVariant=(RecordConstructor names), ..}) = interfaceDeclaration
  where
    interfaceDeclaration = AppE (AppE (ConE 'TSInterfaceDeclaration) (LitE $ StringL $ getConstructorName constructorName)) members
    namesAndTypes :: [(String, Type)] = zip (fmap show names) constructorFields
    members = ListE [(AppE (AppE (AppE (ConE 'TSField)
                                   (ConE 'False))
                             (LitE $ StringL $ lastNameComponent $ nameString))
                       (AppE (VarE 'unTagged) (SigE (VarE 'getTypeScriptType) (AppT (AppT (ConT ''Tagged) typ) (ConT ''String)))))
                    | (nameString, typ) <- namesAndTypes]

getConstructorName :: Name -> String
getConstructorName x = "I" <> lastNameComponent' x

getTypeName :: Name -> String
getTypeName x = lastNameComponent $ show x
