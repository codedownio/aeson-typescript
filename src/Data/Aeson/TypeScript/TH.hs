{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns #-}

module Data.Aeson.TypeScript.TH (
  module Data.Aeson.TypeScript.Instances,
  module Data.Aeson.TypeScript.Types,
  module Data.Aeson.TypeScript.Formatting,
  TSDeclaration(..),
  TSField(..),
  deriveTypeScript
  ) where

import qualified Data.Aeson as A
import Data.Aeson.TypeScript.Formatting
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Types
import Data.Monoid
import Data.String.Interpolate.IsString
import Data.Tagged
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

-- | Generates a 'TypeScript' instance declaration for the given data type or
-- data family instance constructor.
deriveTypeScript :: A.Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance
                 -- declaration.
                 -> Q [Dec]
deriveTypeScript options name = do
  DatatypeInfo {..} <- reifyDatatype name

  let getTypeFn = FunD 'getTypeScriptType [Clause [] (NormalB $ AppE (ConE 'Tagged) (LitE $ StringL $ getTypeName datatypeName)) []]

  declarationFnBody <- case A.sumEncoding options of
    A.TaggedObject _ _ | A.allNullaryToStringTag options && (allConstructorsAreNullary datatypeCons) -> do
      -- Since all constructors are nullary, just encode them to strings
      let strings = [[i|"#{getTypeName $ constructorName x}"|] | x <- datatypeCons]
      let typeDeclaration = AppE (AppE (ConE 'TSTypeAlternatives) (LitE $ StringL $ getTypeName datatypeName)) (ListE [LitE $ StringL s | s <- strings])
      -- Return the single type declaration
      return $ NormalB $ AppE (ConE 'Tagged) (ListE [typeDeclaration])

    A.TaggedObject tagFieldName contentsFieldName -> do
      -- Do tagged object encoding: for each constructor, create an interface. So
      -- data Foo = Foo { fooString :: String } | Bar { barInt :: Int } becomes
      -- type Foo = IFoo | IBar;
      -- interface IFoo { fooString: "string" }
      -- interface IBar { barInt: "number" }

      -- Get the type declaration
      let interfaceNames = ListE [LitE $ StringL $ getConstructorName x | x <- fmap constructorName datatypeCons]
      let typeDeclaration = AppE (AppE (ConE 'TSTypeAlternatives) (LitE $ StringL $ getTypeName datatypeName)) interfaceNames

      -- Get the interface declaration
      let interfaceDeclarations = fmap (getTaggedObjectConstructorDeclaration tagFieldName contentsFieldName options) datatypeCons

      -- Return all the declarations
      return $ NormalB $ AppE (ConE 'Tagged) (ListE (typeDeclaration : interfaceDeclarations))

    A.UntaggedValue -> do
      -- Constructor names won't be encoded. Instead only the contents of the constructor will be encoded as if the type had a single constructor.
      error [i|UntaggedValue not implemented|]
    A.ObjectWithSingleField -> error [i|ObjectWithSingleField not implemented|]
    A.TwoElemArray -> error [i|TwoElemArray not implemented|]

  let getDeclarationFn = FunD 'getTypeScriptDeclaration [Clause [] declarationFnBody []]

  return $ [InstanceD Nothing [] (AppT (ConT ''TypeScript) (ConT name)) [getTypeFn, getDeclarationFn]]

-- | Return an expression that evaluates to a TSInterfaceDeclaration
getTaggedObjectConstructorDeclaration :: String -> String -> A.Options -> ConstructorInfo -> Exp
getTaggedObjectConstructorDeclaration tagFieldName _contentsFieldName options (ConstructorInfo {constructorVariant=(RecordConstructor names), ..}) = interfaceDeclaration
  where
    interfaceDeclaration = AppE (AppE (ConE 'TSInterfaceDeclaration) (LitE $ StringL $ getConstructorName constructorName)) members
    namesAndTypes :: [(String, Type)] = (tagFieldName, (ConT ''String)) : (zip (fmap ((A.fieldLabelModifier options) . show) names) constructorFields)
    members = getTSFields namesAndTypes
getTaggedObjectConstructorDeclaration tagFieldName _ _ (ConstructorInfo {constructorVariant=NormalConstructor, ..}) = interfaceDeclaration
  where
    interfaceDeclaration = AppE (AppE (ConE 'TSInterfaceDeclaration) (LitE $ StringL $ getConstructorName constructorName)) members
    namesAndTypes :: [(String, Type)] = [(tagFieldName, (ConT ''String))]
    members = getTSFields namesAndTypes
getTaggedObjectConstructorDeclaration _tagFieldName _contentsFieldName _ (ConstructorInfo {constructorVariant=x, ..}) = error [i|Constructor variant not supported yet: #{x}|]

getTSFields :: [(String, Type)] -> Exp
getTSFields namesAndTypes = ListE [(AppE (AppE (AppE (ConE 'TSField)
                                                (AppE (VarE 'unTagged) (SigE (VarE 'getTypeScriptOptional) (AppT (AppT (ConT ''Tagged) typ) (ConT ''Bool)))))
                                          (LitE $ StringL $ lastNameComponent $ nameString))
                                    (AppE (VarE 'unTagged) (SigE (VarE 'getTypeScriptType) (AppT (AppT (ConT ''Tagged) typ) (ConT ''String)))))
                                  | (nameString, typ) <- namesAndTypes]

-- * Util stuff

lastNameComponent :: String -> String
lastNameComponent x = T.unpack $ last $ T.splitOn "." (T.pack x)

lastNameComponent' :: Name -> String
lastNameComponent' = lastNameComponent . show

getConstructorName :: Name -> String
getConstructorName x = "I" <> lastNameComponent' x

getTypeName :: Name -> String
getTypeName x = lastNameComponent $ show x

allConstructorsAreNullary :: [ConstructorInfo] -> Bool
allConstructorsAreNullary constructors = and $ fmap isConstructorNullary constructors

isConstructorNullary :: ConstructorInfo -> Bool
isConstructorNullary (ConstructorInfo {constructorVariant, constructorFields}) = (constructorVariant == NormalConstructor) && (constructorFields == [])
