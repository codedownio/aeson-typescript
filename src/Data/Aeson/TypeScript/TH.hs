{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns #-}

module Data.Aeson.TypeScript.TH (
  module Data.Aeson.TypeScript.Instances,
  module Data.Aeson.TypeScript.Types,
  module Data.Aeson.TypeScript.Formatting,
  TSDeclaration(..),
  TSField(..),
  deriveTypeScript,
  T(..),
  T1(..),
  T2(..),
  T3(..)
  ) where

import Data.Aeson as A
import Data.Aeson.TypeScript.Formatting
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.TypeReplacement
import Data.Aeson.TypeScript.Types
import Data.List (inits, tails)
import qualified Data.Map as M
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype

import Debug.Trace


data T = T
data T1 = T1
data T2 = T2
data T3 = T3

instance TypeScript T where
  getTypeScriptType _ = "T"

instance TypeScript T1 where
  getTypeScriptType _ = "T1"

instance TypeScript T2 where
  getTypeScriptType _ = "T2"

instance TypeScript T3 where
  getTypeScriptType _ = "T3"


-- | Generates a 'TypeScript' instance declaration for the given data type or
-- data family instance constructor.
deriveTypeScript :: Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance
                 -- declaration.
                 -> Q [Dec]
deriveTypeScript options name = do
  datatypeInfo@(DatatypeInfo {..}) <- reifyDatatype name

  let templateVars :: [Type] = [ConT ''T1, ConT ''T2, ConT ''T3]

  let fromSigT (SigT typ kind) = typ
      fromSigT typ = typ

  let typeReplacementMap :: M.Map Type Type = M.fromList $ zip (fmap fromSigT datatypeVars) (take (length datatypeVars) templateVars)

  let fullyQualifiedDatatypeInfo = (datatypeInfo {datatypeVars = take (length datatypeVars) templateVars
                                                 , datatypeCons = fmap (replaceTypesInConstructor typeReplacementMap) datatypeCons})

  getTypeFn <- getTypeExpression fullyQualifiedDatatypeInfo >>= \expr -> return $ FunD 'getTypeScriptType [Clause [WildP] (NormalB expr) []]

  getDeclarationFn <- getDeclarationFunctionBody options name fullyQualifiedDatatypeInfo

  -- let nameWithTypeVariables = foldl (\x y -> AppT x y) (ConT name) datatypeVars

  let fullyGenericInstance = InstanceD Nothing [] (AppT (ConT ''TypeScript) (ConT name)) [getTypeFn, getDeclarationFn]

  let otherInstances = [] -- [InstanceD Nothing (fmap getDatatypePredicate datatypeVars) (AppT (ConT ''TypeScript) nameWithTypeVariables) [getTypeFn, getDeclarationFn]]

  return $ fullyGenericInstance : otherInstances


getDeclarationFunctionBody options name datatypeInfo@(DatatypeInfo {..}) = do
  -- If name is higher-kinded, add generic variables to the type and interface declarations
  let genericVariables :: [String] = if | length datatypeVars == 1 -> ["T"]
                                        | otherwise -> ["T" <> show i | i <- [1..(length datatypeVars)]]
  let genericVariablesExp = ListE [stringE x | x <- genericVariables]
  let genericBrackets = getGenericBrackets genericVariables

  let allNullary = (allNullaryToStringTag options) && (allConstructorsAreNullary datatypeCons)
  let singleNormalConstructor = (length datatypeCons == 1) && ((constructorVariant $ head datatypeCons) == NormalConstructor)

  let shouldTag = ((length datatypeCons) > 1) || (tagSingleConstructors options)

  declarationFnBody <- case sumEncoding options of
    x | allNullary && shouldTag -> do
      -- Since all constructors are nullary, just encode them to strings
      let strings = [[i|"#{(constructorTagModifier options) $ getTypeName $ constructorName x}"|] | x <- datatypeCons]
      let typeDeclaration = AppE (AppE (AppE (ConE 'TSTypeAlternatives) (stringE $ getTypeName datatypeName)) genericVariablesExp) (ListE [stringE (s <> genericBrackets) | s <- strings])
      -- Return the single type declaration
      return $ NormalB $ ListE [typeDeclaration]

    x | singleNormalConstructor && (((sumEncoding options == TwoElemArray) && (not allNullary)) ||
                                    (sumEncoding options == ObjectWithSingleField) ||
                                    (tagSingleConstructors options == False)) -> do
      -- Encode to a tuple (as a single type synonym)
      let (ConstructorInfo {..}) = head datatypeCons
      let constructor = if (tagSingleConstructors options && sumEncoding options == TwoElemArray) then 'TSTwoElemArray else 'TSTypeAlternatives
      let typeDeclaration = if | sumEncoding options == ObjectWithSingleField && shouldTag -> do
                                   let name = stringE (constructorTagModifier options $ lastNameComponent' constructorName)
                                   let nameAndInterfaceName = TupE [name, getTypeAsStringExp $ getTupleType constructorFields]
                                   applyToArgsE (ConE 'TSObjectWithSingleField) [stringE $ getTypeName datatypeName, genericVariablesExp, ListE [nameAndInterfaceName]]
                               | otherwise -> applyToArgsE (ConE constructor) [stringE $ getTypeName datatypeName, genericVariablesExp, ListE [getTypeAsStringExp $ getTupleType constructorFields]]
      return $ NormalB $ ListE [typeDeclaration]

    x -> do
      let interfaceNames = [stringE (getConstructorName (constructorTagModifier options) x <> genericBrackets) | x <- fmap constructorName datatypeCons]
      let constructor = if | sumEncoding options == TwoElemArray && (tagSingleConstructors options || length datatypeCons > 1) -> 'TSTwoElemArray
                           | otherwise -> 'TSTypeAlternatives

      let typeDeclaration = if | sumEncoding options == ObjectWithSingleField && shouldTag -> do
                                   let names = [stringE (constructorTagModifier options $ lastNameComponent' x) | x <- fmap constructorName datatypeCons]
                                   let namesAndInterfaceNames = ListE $ [TupE [name, interfaceName] | (name, interfaceName) <- zip names interfaceNames]
                                   applyToArgsE (ConE 'TSObjectWithSingleField) [stringE $ getTypeName datatypeName, genericVariablesExp, namesAndInterfaceNames]
                               | otherwise -> applyToArgsE (ConE constructor) [stringE $ getTypeName datatypeName, genericVariablesExp, ListE interfaceNames]

      let interfaceDeclarations = fmap (getSumObjectConstructorDeclaration options shouldTag genericVariables) datatypeCons

      return $ NormalB $ ListE (typeDeclaration : interfaceDeclarations)


  return $ FunD 'getTypeScriptDeclaration [Clause [WildP] declarationFnBody []]


-- | Return an expression that evaluates to a TSInterfaceDeclaration
-- Sum object encoding to TS creates an interface for each constructor. So
-- data Foo = Foo { fooString :: String } | Bar { barInt :: Int } becomes
-- type Foo = IFoo | IBar;
-- interface IFoo { fooString: "string" }
-- interface IBar { barInt: "number" }
-- This function produces a single interface declaration
getSumObjectConstructorDeclaration :: Options -> Bool -> [String] -> ConstructorInfo -> Exp
-- | Handle TaggedObject
getSumObjectConstructorDeclaration options shouldTag genericVariables (ConstructorInfo {constructorVariant=(RecordConstructor names), ..}) = interfaceDeclaration
  where
    fieldNamesAndTypes = zip (fmap ((fieldLabelModifier options) . lastNameComponent') names) constructorFields
    namesAndTypes :: [(String, Type)] = case sumEncoding options of
      TaggedObject tagFieldName contentsFieldName | shouldTag -> (tagFieldName, (ConT ''String)) : fieldNamesAndTypes
      _ -> fieldNamesAndTypes
    interfaceDeclaration = assembleInterfaceDeclaration options constructorName genericVariables (getTSFields namesAndTypes)
getSumObjectConstructorDeclaration options shouldTag genericVariables (ConstructorInfo {constructorVariant=NormalConstructor, ..}) = interfaceDeclaration
  where
    contentsTupleType = getTupleType constructorFields
    namesAndTypes :: [(String, Type)] = case sumEncoding options of
      TaggedObject tagFieldName contentsFieldName | shouldTag -> [(tagFieldName, (ConT ''String)), (contentsFieldName, contentsTupleType)]
      TaggedObject tagFieldName contentsFieldName -> [(contentsFieldName, contentsTupleType)]
      _ -> [(show constructorName, contentsTupleType)]
    interfaceDeclaration = assembleInterfaceDeclaration options constructorName genericVariables (getTSFields namesAndTypes)

getSumObjectConstructorDeclaration  _ _ _ (ConstructorInfo {constructorVariant=x, ..}) = error [i|Constructor variant not supported yet: #{x}|]

-- | Helper for getSumObjectConstructorDeclaration
getTSFields :: [(String, Type)] -> Exp
getTSFields namesAndTypes = ListE [(AppE (AppE (AppE (ConE 'TSField) (getOptionalAsBoolExp typ))
                                           (stringE nameString))
                                    (getTypeAsStringExp typ))
                                  | (nameString, typ) <- namesAndTypes]

-- | Helper for getSumObjectConstructorDeclaration
assembleInterfaceDeclaration options constructorName genericVariables members = AppE (AppE (AppE (ConE 'TSInterfaceDeclaration) constructorNameExp) genericVariablesExp) members where
  constructorNameExp = stringE $ getConstructorName (constructorTagModifier options) constructorName
  genericVariablesExp = (ListE [stringE x | x <- genericVariables])


-- * Getting type expression

-- | Get an expression to be used for getTypeScriptType.
-- For datatypes of kind * this is easy, since we can just evaluate the string literal in TH.
-- For higher-kinded types, we need to make an expression which evaluates the template types and fills it in.
getTypeExpression :: DatatypeInfo -> Q Exp
getTypeExpression (DatatypeInfo {datatypeVars=[], ..}) = return $ stringE $ getTypeName datatypeName
getTypeExpression (DatatypeInfo {datatypeVars=vars, ..}) = do
  let baseName = stringE $ getTypeName datatypeName
  let typeNames = ListE [getTypeAsStringExp typ | typ <- vars]
  let headType = AppE (VarE 'head) typeNames
  let tailType = AppE (VarE 'tail) typeNames
  let comma = stringE ", "
  x <- newName "x"
  let tailsWithCommas = AppE (VarE 'mconcat) (CompE [BindS (VarP x) tailType, NoBindS (AppE (AppE (VarE 'mappend) comma) (VarE x))])
  let brackets = AppE (VarE 'mconcat) (ListE [stringE "<", headType, tailsWithCommas, stringE ">"])

  return $ (AppE (AppE (VarE 'mappend) baseName) brackets)

-- * Util stuff

lastNameComponent :: String -> String
lastNameComponent x = T.unpack $ last $ T.splitOn "." (T.pack x)

lastNameComponent' :: Name -> String
lastNameComponent' = lastNameComponent . show

getConstructorName :: (String -> String) -> Name -> String
getConstructorName constructorTagModifier x = "I" <> (constructorTagModifier $ lastNameComponent' x)

getTypeName :: Name -> String
getTypeName x = lastNameComponent $ show x

allConstructorsAreNullary :: [ConstructorInfo] -> Bool
allConstructorsAreNullary constructors = and $ fmap isConstructorNullary constructors

isConstructorNullary :: ConstructorInfo -> Bool
isConstructorNullary (ConstructorInfo {constructorVariant, constructorFields}) = (constructorVariant == NormalConstructor) && (constructorFields == [])

getDatatypePredicate :: Type -> Pred
getDatatypePredicate typ = AppT (ConT ''TypeScript) typ

getTypeAsStringExp :: Type -> Exp
getTypeAsStringExp typ = AppE (VarE 'getTypeScriptType) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ))

getOptionalAsBoolExp :: Type -> Exp
getOptionalAsBoolExp typ = AppE (VarE 'getTypeScriptOptional) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ))

-- | Get the type of a tuple of constructor fields, as when we're packing a record-less constructor into a list
getTupleType constructorFields = case length constructorFields of
  0 -> AppT ListT (ConT ''())
  1 -> head constructorFields
  x -> applyToArgsT (ConT $ tupleTypeName x) constructorFields

-- | Helper to apply a type constructor to a list of type args
applyToArgsT :: Type -> [Type] -> Type
applyToArgsT constructor [] = constructor
applyToArgsT constructor (x:xs) = applyToArgsT (AppT constructor x) xs

-- | Helper to apply a function a list of args
applyToArgsE :: Exp -> [Exp] -> Exp
applyToArgsE f [] = f
applyToArgsE f (x:xs) = applyToArgsE (AppE f x) xs

stringE = LitE . StringL

unitSynonym :: ()
unitSynonym = ()
