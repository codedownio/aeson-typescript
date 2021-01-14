{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns, PolyKinds #-}

{-|
Module:      Data.Aeson.TypeScript.TH
Copyright:   (c) 2018 Tom McLaughlin
License:     BSD3
Stability:   experimental
Portability: portable

This library provides a way to generate TypeScript @.d.ts@ files that match your existing Aeson 'A.ToJSON' instances.
If you already use Aeson's Template Haskell support to derive your instances, then deriving TypeScript is as simple as

@
$('deriveTypeScript' myAesonOptions ''MyType)
@

For example,

@
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq
@

Next we derive the necessary instances.

@
$('deriveTypeScript' ('defaultOptions' {'fieldLabelModifier' = 'drop' 4, 'constructorTagModifier' = map toLower}) ''D)
@

Now we can use the newly created instances.

@
>>> putStrLn $ 'formatTSDeclarations' $ 'getTypeScriptDeclarations' (Proxy :: Proxy D)

type D\<T\> = INullary\<T\> | IUnary\<T\> | IProduct\<T\> | IRecord\<T\>;

interface INullary\<T\> {
  tag: "nullary";
}

interface IUnary\<T\> {
  tag: "unary";
  contents: number;
}

interface IProduct\<T\> {
  tag: "product";
  contents: [string, string, T];
}

interface IRecord\<T\> {
  tag: "record";
  One: number;
  Two: boolean;
  Three: D\<T\>;
}
@

It's important to make sure your JSON and TypeScript are being derived with the same options. For this reason, we
include the convenience 'HasJSONOptions' typeclass, which lets you write the options only once, like this:

@
instance HasJSONOptions MyType where getJSONOptions _ = ('defaultOptions' {'fieldLabelModifier' = 'drop' 4})

$('deriveJSON' ('getJSONOptions' (Proxy :: Proxy MyType)) ''MyType)
$('deriveTypeScript' ('getJSONOptions' (Proxy :: Proxy MyType)) ''MyType)
@

Or, if you want to be even more concise and don't mind defining the instances in the same file,

@
myOptions = 'defaultOptions' {'fieldLabelModifier' = 'drop' 4}

$('deriveJSONAndTypeScript' myOptions ''MyType)
@

Remembering that the Template Haskell 'Q' monad is an ordinary monad, you can derive instances for several types at once like this:

@
$('mconcat' \<$\> 'traverse' ('deriveJSONAndTypeScript' myOptions) [''MyType1, ''MyType2, ''MyType3])
@

Once you've defined all necessary instances, you can write a main function to dump them out into a @.d.ts@ file. For example:

@
main = putStrLn $ 'formatTSDeclarations' (
  ('getTypeScriptDeclarations' (Proxy :: Proxy MyType1)) <>
  ('getTypeScriptDeclarations' (Proxy :: Proxy MyType2)) <>
  ...
)
@

-}

module Data.Aeson.TypeScript.TH (
  deriveTypeScript,

  -- * The main typeclass
  TypeScript(..),
  TSType(..),

  TSDeclaration(TSRawDeclaration),

  -- * Formatting declarations
  formatTSDeclarations,
  formatTSDeclarations',
  formatTSDeclaration,
  FormattingOptions(..),

  -- * Convenience tools
  HasJSONOptions(..),
  deriveJSONAndTypeScript,

  T(..),
  F1(..),
  F2(..),
  F3(..), -- TODO: expose the rest of these if necessary
    
  module Data.Aeson.TypeScript.Instances
  ) where

import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.Formatting
import Data.Aeson.TypeScript.Types
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Util
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype


-- | Generates a 'TypeScript' instance declaration for the given data type.
deriveTypeScript :: Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                 -> Q [Dec]
deriveTypeScript options name = do
  datatypeInfo@(DatatypeInfo {..}) <- reifyDatatype name

  -- reportWarning [i|Reified: #{datatypeInfo}|]

  assertExtensionsTurnedOn datatypeInfo

  let getFreeVariableName (SigT (VarT n) _kind) = Just n
      getFreeVariableName _ = Nothing

  let templateVarsToUse = case datatypeVars of
        [KindedTV _ StarT] -> [ConT ''T]
        vars -> chooseDataTypeVars allStarConstructors allPolyStarConstructors vars

  let subMap = M.fromList $ zip (mapMaybe getFreeVariableName (getDataTypeVars datatypeInfo)) templateVarsToUse

  -- reportWarning [i|subMap: #{subMap}|]

  let fullyQualifiedDatatypeInfo = setDataTypeVars (datatypeInfo { datatypeCons = fmap (applySubstitution subMap) datatypeCons}) templateVarsToUse

  getTypeFn <- getTypeExpression fullyQualifiedDatatypeInfo >>= \expr -> return $ FunD 'getTypeScriptType [Clause [WildP] (NormalB expr) []]
  getDeclarationFn <- getDeclarationFunctionBody options name fullyQualifiedDatatypeInfo
  getGenericParentTypesFn <- getGenericParentTypesExpression fullyQualifiedDatatypeInfo >>= \expr -> return $ FunD 'getParentTypes [Clause [WildP] (NormalB expr) []]
  getNonGenericParentTypesFn <- getNonGenericParentTypesExpression fullyQualifiedDatatypeInfo >>= \expr -> return $ FunD 'getParentTypes [Clause [WildP] (NormalB expr) []]

  let fullyGenericInstance = mkInstance [] (AppT (ConT ''TypeScript) (ConT name)) [getTypeFn, getDeclarationFn, getGenericParentTypesFn]

  otherInstances <- case null datatypeVars of
    False -> do
      otherGetTypeFn <- getTypeExpression datatypeInfo >>= \expr -> return $ FunD 'getTypeScriptType [Clause [WildP] (NormalB expr) []]
      return [mkInstance (fmap getDatatypePredicate (getDataTypeVars datatypeInfo)) (AppT (ConT ''TypeScript) (foldl AppT (ConT name) (getDataTypeVars datatypeInfo))) [otherGetTypeFn, getNonGenericParentTypesFn]]
    True -> return []

  return $ fullyGenericInstance : otherInstances

getDeclarationFunctionBody :: Options -> p -> DatatypeInfo -> Q Dec
getDeclarationFunctionBody options _name datatypeInfo@(DatatypeInfo {..}) = do
  -- If name is higher-kinded, add generic variables to the type and interface declarations
  let genericVariables :: [String] = if | length datatypeVars == 1 -> ["T"]
                                        | otherwise -> ["T" <> show j | j <- [1..(length datatypeVars)]]
  let genericVariablesExp = ListE [stringE x | x <- genericVariables]

  declarationFnBody <- do
    let interfaceNamesAndDeclarations = fmap (handleConstructor options datatypeInfo genericVariables) datatypeCons
    let interfaceDeclarations = catMaybes $ fmap snd3 interfaceNamesAndDeclarations

    case interfaceNamesAndDeclarations of
      [(_, Just interfaceDecl, True)] | datatypeVars == [] -> do
        -- The type declaration is just a reference to a single interface, so we can omit the type part and drop the "I" from the interface name
        return $ NormalB $ ListE [AppE (VarE 'dropLeadingIFromInterfaceName) interfaceDecl]

      _ -> do
        let interfaceNames = fmap fst3 interfaceNamesAndDeclarations

        let typeDeclaration = applyToArgsE (ConE 'TSTypeAlternatives) [stringE $ getTypeName datatypeName, genericVariablesExp, ListE interfaceNames]
        return $ NormalB $ ListE (typeDeclaration : interfaceDeclarations)

  return $ FunD 'getTypeScriptDeclarations [Clause [WildP] declarationFnBody []]


-- | Return a string to go in the top-level type declaration, plus an optional expression containing a declaration
handleConstructor :: Options -> DatatypeInfo -> [String] -> ConstructorInfo -> (Exp, Maybe Exp, Bool)
handleConstructor options (DatatypeInfo {..}) genericVariables ci@(ConstructorInfo {}) =
  if | isSingleConstructorType && not (getTagSingleConstructors options) -> (stringE interfaceNameWithBrackets, singleConstructorEncoding, True)

     | allConstructorsAreNullary datatypeCons && allNullaryToStringTag options -> stringEncoding

     -- With UntaggedValue, nullary constructors are encoded as strings
     | (isUntaggedValue $ sumEncoding options) && isConstructorNullary ci -> stringEncoding

     -- Treat as a sum
     | isObjectWithSingleField $ sumEncoding options -> (stringE [i|{#{show constructorNameToUse}: #{interfaceNameWithBrackets}}|], singleConstructorEncoding, False)
     | isTwoElemArray $ sumEncoding options -> (stringE [i|[#{show constructorNameToUse}, #{interfaceNameWithBrackets}]|], singleConstructorEncoding, False)
     | isUntaggedValue $ sumEncoding options -> (stringE interfaceNameWithBrackets, singleConstructorEncoding, True)
     | otherwise -> (stringE interfaceNameWithBrackets, taggedConstructorEncoding, True)

  where
    stringEncoding = (stringE [i|"#{(constructorTagModifier options) $ getTypeName (constructorName ci)}"|], Nothing, True)

    singleConstructorEncoding = if | constructorVariant ci == NormalConstructor -> tupleEncoding
                                   | otherwise -> Just $ assembleInterfaceDeclaration (ListE (getTSFields options namesAndTypes))

    taggedConstructorEncoding = Just $ assembleInterfaceDeclaration (ListE (tagField ++ getTSFields options namesAndTypes))

    -- * Type declaration to use
    interfaceName = getInterfaceName ci
    interfaceNameWithBrackets = interfaceName <> getGenericBrackets genericVariables

    tupleEncoding = Just $ applyToArgsE (ConE 'TSTypeAlternatives) [stringE $ interfaceName
                                                                   , ListE [stringE x | x <- genericVariables]
                                                                   , ListE [getTypeAsStringExp contentsTupleType]]

    namesAndTypes :: [(String, Type)] = case constructorVariant ci of
      RecordConstructor names -> zip (fmap ((fieldLabelModifier options) . lastNameComponent') names) (constructorFields ci)
      NormalConstructor -> case sumEncoding options of
        TaggedObject _ contentsFieldName -> if | isConstructorNullary ci -> []
                                                          | otherwise -> [(contentsFieldName, contentsTupleType)]
        _ -> [(constructorNameToUse, contentsTupleType)]

    tagField = case sumEncoding options of
      TaggedObject tagFieldName _ -> [(AppE (AppE (AppE (ConE 'TSField) (ConE 'False))
                                              (stringE tagFieldName))
                                        (stringE [i|"#{constructorNameToUse}"|]))]
      _ -> []

    isSingleConstructorType = length datatypeCons == 1

    getInterfaceName (constructorName -> x) = "I" <> (lastNameComponent' x)

    constructorNameToUse = (constructorTagModifier options) $ lastNameComponent' (constructorName ci)
    contentsTupleType = getTupleType (constructorFields ci)

    assembleInterfaceDeclaration members = AppE (AppE (AppE (ConE 'TSInterfaceDeclaration) (stringE interfaceName)) genericVariablesExp) members where
      genericVariablesExp = (ListE [stringE x | x <- genericVariables])


-- | Helper for handleConstructor
getTSFields :: Options -> [(String, Type)] -> [Exp]
getTSFields options namesAndTypes =
  [ (AppE (AppE (AppE (ConE 'TSField) optAsBool)
             (stringE nameString))
       fieldTyp)
  | (nameString, typ) <- namesAndTypes
  , let (fieldTyp, optAsBool) = getFieldType options typ]

getFieldType :: Options -> Type -> (Exp, Exp)
getFieldType options (AppT (ConT name) t)
  | not (omitNothingFields options) && name == ''Maybe
  = (AppE (AppE (VarE 'mappend) (getTypeAsStringExp t)) (stringE " | null"), getOptionalAsBoolExp t)
getFieldType _ typ = (getTypeAsStringExp typ, getOptionalAsBoolExp typ)


-- * Getting type expression

-- | Get an expression to be used for getTypeScriptType.
-- For datatypes of kind * this is easy, since we can just evaluate the string literal in TH.
-- For higher-kinded types, we need to make an expression which evaluates the template types and fills it in.
getTypeExpression :: DatatypeInfo -> Q Exp
getTypeExpression di@(getDataTypeVars -> []) = return $ stringE $ getTypeName (datatypeName di)
getTypeExpression di@(getDataTypeVars -> vars) = do
  let baseName = getTypeName (datatypeName di)
  let typeNames = ListE [getTypeAsStringExp typ | typ <- vars]
  [|$(return (stringE baseName)) <> "<" <> (L.intercalate ", " $(return typeNames)) <> ">"|]

-- * Convenience functions

-- | Convenience function to generate 'A.ToJSON', 'A.FromJSON', and 'TypeScript' instances simultaneously, so the instances are guaranteed to be in sync.
--
-- This function is given mainly as an illustration. If you want some other permutation of instances, such as 'A.ToJSON' and 'A.TypeScript' only, just take a look at the source and write your own version.
--
-- @since 0.1.0.4
deriveJSONAndTypeScript :: Options
                        -- ^ Encoding options.
                        -> Name
                        -- ^ Name of the type for which to generate 'A.ToJSON', 'A.FromJSON', and 'TypeScript' instance declarations.
                        -> Q [Dec]
deriveJSONAndTypeScript options name = (<>) <$> (deriveTypeScript options name) <*> (A.deriveJSON options name)

-- | For the fully generic instance, the parent types are the types inside the constructors
getGenericParentTypesExpression :: DatatypeInfo -> Q Exp
getGenericParentTypesExpression (DatatypeInfo {..}) = return $ ListE [AppE (ConE 'TSType) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ)) | typ <- types]
  where types = mconcat $ fmap constructorFields datatypeCons

-- | For the non-generic instances, the parent type is the generic type
getNonGenericParentTypesExpression :: DatatypeInfo -> Q Exp
getNonGenericParentTypesExpression (DatatypeInfo {..}) = return $ ListE [AppE (ConE 'TSType) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (ConT datatypeName)))]


chooseDataTypeVars _ _ [] = []
chooseDataTypeVars starConstructors polyStarConstructors (x:xs) = case x of
  PlainTV _ -> (head starConstructors) : chooseDataTypeVars (tail starConstructors) polyStarConstructors xs
  KindedTV _ StarT -> (head starConstructors) : chooseDataTypeVars (tail starConstructors) polyStarConstructors xs
  -- higher -> (higher) : chooseDataTypeVars starConstructors (tail polyStarConstructors) xs
  _ -> (head allPolyStarConstructors) : chooseDataTypeVars starConstructors (tail polyStarConstructors) xs
