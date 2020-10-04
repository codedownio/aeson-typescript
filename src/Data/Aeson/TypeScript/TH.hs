{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns #-}

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

  TSDeclaration,

  -- * Formatting declarations
  formatTSDeclarations,
  formatTSDeclarations',
  formatTSDeclaration,
  FormattingOptions(..),
  SumTypeFormat(..),

  -- * Convenience tools
  HasJSONOptions(..),
  deriveJSONAndTypeScript,

  module Data.Aeson.TypeScript.Instances
  ) where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.Formatting
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Types
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype

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

instance TypeScript T where getTypeScriptType _ = "T"
instance TypeScript T1 where getTypeScriptType _ = "T1"
instance TypeScript T2 where getTypeScriptType _ = "T2"
instance TypeScript T3 where getTypeScriptType _ = "T3"
instance TypeScript T4 where getTypeScriptType _ = "T4"
instance TypeScript T5 where getTypeScriptType _ = "T5"
instance TypeScript T6 where getTypeScriptType _ = "T6"
instance TypeScript T7 where getTypeScriptType _ = "T7"
instance TypeScript T8 where getTypeScriptType _ = "T8"
instance TypeScript T9 where getTypeScriptType _ = "T9"
instance TypeScript T10 where getTypeScriptType _ = "T10"


-- | Generates a 'TypeScript' instance declaration for the given data type.
deriveTypeScript :: Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                 -> Q [Dec]
deriveTypeScript options name = do
  datatypeInfo@(DatatypeInfo {..}) <- reifyDatatype name

  assertExtensionsTurnedOn datatypeInfo

  let getFreeVariableName (SigT (VarT n) _kind) = Just n
      getFreeVariableName _ = Nothing

  let templateVarsToUse = case length datatypeVars of
        1 -> [ConT ''T]
        _ -> take (length datatypeVars) [ConT ''T1, ConT ''T2, ConT ''T3, ConT ''T4, ConT ''T5, ConT ''T6, ConT ''T7, ConT ''T8, ConT ''T9, ConT ''T10]

#if MIN_VERSION_th_abstraction(0,3,0)
  let subMap = M.fromList $ zip (mapMaybe getFreeVariableName datatypeInstTypes) templateVarsToUse
  let fullyQualifiedDatatypeInfo = (datatypeInfo {datatypeInstTypes = templateVarsToUse
                                                 , datatypeCons = fmap (applySubstitution subMap) datatypeCons})
#else
  let subMap = M.fromList $ zip (mapMaybe getFreeVariableName datatypeVars) templateVarsToUse
  let fullyQualifiedDatatypeInfo = (datatypeInfo {datatypeVars = templateVarsToUse
                                                 , datatypeCons = fmap (applySubstitution subMap) datatypeCons})
#endif
  getTypeFn <- getTypeExpression fullyQualifiedDatatypeInfo >>= \expr -> return $ FunD 'getTypeScriptType [Clause [WildP] (NormalB expr) []]
  getDeclarationFn <- getDeclarationFunctionBody options name fullyQualifiedDatatypeInfo
  getGenericParentTypesFn <- getGenericParentTypesExpression fullyQualifiedDatatypeInfo >>= \expr -> return $ FunD 'getParentTypes [Clause [WildP] (NormalB expr) []]
  getNonGenericParentTypesFn <- getNonGenericParentTypesExpression fullyQualifiedDatatypeInfo >>= \expr -> return $ FunD 'getParentTypes [Clause [WildP] (NormalB expr) []]

  let fullyGenericInstance = mkInstance [] (AppT (ConT ''TypeScript) (ConT name)) [getTypeFn, getDeclarationFn, getGenericParentTypesFn]

  otherInstances <- case null datatypeVars of
    False -> do
      otherGetTypeFn <- getTypeExpression datatypeInfo >>= \expr -> return $ FunD 'getTypeScriptType [Clause [WildP] (NormalB expr) []]
#if MIN_VERSION_th_abstraction(0,3,0)
      return [mkInstance (fmap getDatatypePredicate datatypeInstTypes) (AppT (ConT ''TypeScript) (foldl AppT (ConT name) datatypeInstTypes)) [otherGetTypeFn, getNonGenericParentTypesFn]]
#else
      return [mkInstance (fmap getDatatypePredicate datatypeVars) (AppT (ConT ''TypeScript) (foldl AppT (ConT name) datatypeVars)) [otherGetTypeFn, getNonGenericParentTypesFn]]
#endif
    True -> return []

  return $ fullyGenericInstance : otherInstances

-- | For the fully generic instance, the parent types are the types inside the constructors
getGenericParentTypesExpression :: DatatypeInfo -> Q Exp
getGenericParentTypesExpression (DatatypeInfo {..}) = return $ ListE [AppE (ConE 'TSType) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ)) | typ <- types]
  where types = mconcat $ fmap constructorFields $ datatypeCons

-- | For the non-generic instances, the parent type is the generic type
getNonGenericParentTypesExpression :: DatatypeInfo -> Q Exp
getNonGenericParentTypesExpression (DatatypeInfo {..}) = return $ ListE [AppE (ConE 'TSType) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (ConT datatypeName)))]

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

dropLeadingIFromInterfaceName :: TSDeclaration -> TSDeclaration
dropLeadingIFromInterfaceName decl@(TSInterfaceDeclaration {interfaceName=('I':xs)}) = decl { interfaceName = xs }
dropLeadingIFromInterfaceName decl@(TSTypeAlternatives {typeName=('I':xs)}) = decl { typeName = xs }
dropLeadingIFromInterfaceName x = x

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
#if MIN_VERSION_th_abstraction(0,3,0)
getTypeExpression :: DatatypeInfo -> Q Exp
getTypeExpression (DatatypeInfo {datatypeInstTypes=[], ..}) = return $ stringE $ getTypeName datatypeName
getTypeExpression (DatatypeInfo {datatypeInstTypes=vars, ..}) = do
#else
getTypeExpression :: DatatypeInfo -> Q Exp
getTypeExpression (DatatypeInfo {datatypeVars=[], ..}) = return $ stringE $ getTypeName datatypeName
getTypeExpression (DatatypeInfo {datatypeVars=vars, ..}) = do
#endif
  let baseName = stringE $ getTypeName datatypeName
  let typeNames = ListE [getTypeAsStringExp typ | typ <- vars]
  let headType = AppE (VarE 'head) typeNames
  let tailType = AppE (VarE 'tail) typeNames
  let comma = stringE ", "
  x <- newName "x"
  let tailsWithCommas = AppE (VarE 'mconcat) (CompE [BindS (VarP x) tailType, NoBindS (AppE (AppE (VarE 'mappend) comma) (VarE x))])
  let brackets = AppE (VarE 'mconcat) (ListE [stringE "<", headType, tailsWithCommas, stringE ">"])

  return $ (AppE (AppE (VarE 'mappend) baseName) brackets)

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
deriveJSONAndTypeScript options name = do
  ts <- deriveTypeScript options name
  json <- A.deriveJSON options name
  return $ ts <> json

-- * Util stuff

lastNameComponent :: String -> String
lastNameComponent x = T.unpack $ last $ T.splitOn "." (T.pack x)

lastNameComponent' :: Name -> String
lastNameComponent' = lastNameComponent . show

getTypeName :: Name -> String
getTypeName x = lastNameComponent $ show x

allConstructorsAreNullary :: [ConstructorInfo] -> Bool
allConstructorsAreNullary constructors = and $ fmap isConstructorNullary constructors

isConstructorNullary :: ConstructorInfo -> Bool
isConstructorNullary (ConstructorInfo {constructorVariant, constructorFields}) = (constructorVariant == NormalConstructor) && (constructorFields == [])

-- In Template Haskell 2.10.0.0 and later, Pred is just a synonm for Type
-- In earlier versions, it has constructors
getDatatypePredicate :: Type -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
getDatatypePredicate typ = AppT (ConT ''TypeScript) typ
#else
getDatatypePredicate typ = ClassP ''TypeScript [typ]
#endif

getTypeAsStringExp :: Type -> Exp
getTypeAsStringExp typ = AppE (VarE 'getTypeScriptType) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ))

getOptionalAsBoolExp :: Type -> Exp
getOptionalAsBoolExp typ = AppE (VarE 'getTypeScriptOptional) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ))

isTaggedObject (sumEncoding -> TaggedObject _ _) = True
isTaggedObject _ = False

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

-- Between Template Haskell 2.10 and 2.11, InstanceD got an additional argument
#if MIN_VERSION_template_haskell(2,11,0)
mkInstance context typ decs = InstanceD Nothing context typ decs
#else
mkInstance context typ decs = InstanceD context typ decs
#endif

-- Between Aeson 1.1.2.0 and 1.2.0.0, tagSingleConstructors was added
getTagSingleConstructors :: Options -> Bool
#if MIN_VERSION_aeson(1,2,0)
getTagSingleConstructors options = tagSingleConstructors options
#else
getTagSingleConstructors _ = False
#endif

-- Between Template Haskell 2.10 and 2.11, the ability to look up which extensions are turned on was added
assertExtensionsTurnedOn :: DatatypeInfo -> Q ()
#if MIN_VERSION_template_haskell(2,11,0)
assertExtensionsTurnedOn (DatatypeInfo {..}) = do
  -- Check that necessary language extensions are turned on
  scopedTypeVariablesEnabled <- isExtEnabled ScopedTypeVariables
  kindSignaturesEnabled <- isExtEnabled KindSignatures
  when (not scopedTypeVariablesEnabled) $ error [i|The ScopedTypeVariables extension is required; please enable it before calling deriveTypeScript. (For example: put {-# LANGUAGE ScopedTypeVariables #-} at the top of the file.)|]
  when ((not kindSignaturesEnabled) && (length datatypeVars > 0)) $ error [i|The KindSignatures extension is required since type #{datatypeName} is a higher order type; please enable it before calling deriveTypeScript. (For example: put {-# LANGUAGE KindSignatures #-} at the top of the file.)|]
#else
assertExtensionsTurnedOn _ = return ()
#endif

-- Older versions of Aeson don't have an Eq instance for SumEncoding so we do this
isObjectWithSingleField ObjectWithSingleField = True
isObjectWithSingleField _ = False

-- Older versions of Aeson don't have an Eq instance for SumEncoding so we do this
isTwoElemArray TwoElemArray = True
isTwoElemArray _ = False

-- Older versions of Aeson don't have an Eq instance for SumEncoding so we do this
-- UntaggedValue was added between Aeson 0.11.3.0 and 1.0.0.0
#if MIN_VERSION_aeson(1,0,0)
isUntaggedValue UntaggedValue = True
#endif
isUntaggedValue _ = False

fst3 (x, _, _) = x
snd3 (_, y, _) = y
thd3 (_, _, z) = z
