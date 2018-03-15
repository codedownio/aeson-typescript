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

type D\<T\> = "nullary" | IUnary\<T\> | IProduct\<T\> | IRecord\<T\>;

type IUnary\<T\> = number;

type IProduct\<T\> = [string, string, T];

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

  TSDeclaration,

  -- * Formatting declarations
  formatTSDeclarations,
  formatTSDeclarations',
  formatTSDeclaration,
  FormattingOptions(..),

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
import Data.Aeson.Types as A
import Data.List (inits, tails)
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

instance TypeScript T where
  getTypeScriptType _ = "T"

instance TypeScript T1 where
  getTypeScriptType _ = "T1"

instance TypeScript T2 where
  getTypeScriptType _ = "T2"

instance TypeScript T3 where
  getTypeScriptType _ = "T3"

instance TypeScript T4 where
  getTypeScriptType _ = "T4"

instance TypeScript T5 where
  getTypeScriptType _ = "T5"

instance TypeScript T6 where
  getTypeScriptType _ = "T6"

instance TypeScript T7 where
  getTypeScriptType _ = "T7"

instance TypeScript T8 where
  getTypeScriptType _ = "T8"

instance TypeScript T9 where
  getTypeScriptType _ = "T9"

instance TypeScript T10 where
  getTypeScriptType _ = "T10"


-- | Generates a 'TypeScript' instance declaration for the given data type.
deriveTypeScript :: Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                 -> Q [Dec]
deriveTypeScript options name = do
  datatypeInfo@(DatatypeInfo {..}) <- reifyDatatype name

  assertExtensionsTurnedOn datatypeInfo

  let getFreeVariableName (SigT (VarT name) kind) = Just name
      getFreeVariableName typ = Nothing

  let templateVarsToUse = case length datatypeVars of
        1 -> [ConT ''T]
        n -> take (length datatypeVars) [ConT ''T1, ConT ''T2, ConT ''T3, ConT ''T4, ConT ''T5, ConT ''T6, ConT ''T7, ConT ''T8, ConT ''T9, ConT ''T10]

  let subMap = M.fromList $ zip (catMaybes $ fmap getFreeVariableName datatypeVars) templateVarsToUse
  let fullyQualifiedDatatypeInfo = (datatypeInfo {datatypeVars = templateVarsToUse
                                                 , datatypeCons = fmap (applySubstitution subMap) datatypeCons})
  getTypeFn <- getTypeExpression fullyQualifiedDatatypeInfo >>= \expr -> return $ FunD 'getTypeScriptType [Clause [WildP] (NormalB expr) []]
  getDeclarationFn <- getDeclarationFunctionBody options name fullyQualifiedDatatypeInfo
  let fullyGenericInstance = mkInstance [] (AppT (ConT ''TypeScript) (ConT name)) [getTypeFn, getDeclarationFn]

  otherInstances <- case length datatypeVars > 0 of
    True -> do
      otherGetTypeFn <- getTypeExpression datatypeInfo >>= \expr -> return $ FunD 'getTypeScriptType [Clause [WildP] (NormalB expr) []]
      return [mkInstance (fmap getDatatypePredicate datatypeVars) (AppT (ConT ''TypeScript) (foldl (\x y -> AppT x y) (ConT name) datatypeVars)) [otherGetTypeFn]]
    False -> return []

  return $ fullyGenericInstance : otherInstances

getDeclarationFunctionBody :: Options -> p -> DatatypeInfo -> Q Dec
getDeclarationFunctionBody options _name datatypeInfo@(DatatypeInfo {..}) = do
  -- If name is higher-kinded, add generic variables to the type and interface declarations
  let genericVariables :: [String] = if | length datatypeVars == 1 -> ["T"]
                                        | otherwise -> ["T" <> show i | i <- [1..(length datatypeVars)]]
  let genericVariablesExp = ListE [stringE x | x <- genericVariables]

  let allNullary = (allNullaryToStringTag options) && (allConstructorsAreNullary datatypeCons)
  let singleNormalConstructor = (length datatypeCons == 1) && ((constructorVariant $ head datatypeCons) == NormalConstructor)

  declarationFnBody <- do
    let interfaceNamesAndDeclarations = fmap (handleConstructor options datatypeInfo genericVariables) datatypeCons
    let interfaceNames = fmap fst interfaceNamesAndDeclarations
    let interfaceDeclarations = catMaybes $ fmap snd interfaceNamesAndDeclarations

    let typeDeclaration = applyToArgsE (ConE 'TSTypeAlternatives) [stringE $ getTypeName datatypeName, genericVariablesExp, ListE interfaceNames]

    return $ NormalB $ ListE (typeDeclaration : interfaceDeclarations)

  return $ FunD 'getTypeScriptDeclarations [Clause [WildP] declarationFnBody []]


-- | Return a string to go in the top-level type declaration, plus an optional expression containing a declaration
handleConstructor :: Options -> DatatypeInfo -> [String] -> ConstructorInfo -> (Exp, Maybe Exp)
handleConstructor options (DatatypeInfo {..}) genericVariables (ConstructorInfo {..}) = (typeDeclarationToUse, declaration)
  where
    -- * Type declaration to use
    interfaceName = getInterfaceName constructorName <> (getGenericBrackets genericVariables)
    typeDeclarationToUse = if | shouldEncodeToString -> stringE [i|"#{(constructorTagModifier options) $ getTypeName $ constructorName}"|]
                              | (isObjectWithSingleField $ sumEncoding options) && shouldTag -> stringE [i|{#{show constructorNameToUse}: #{interfaceName}}|]
                              | (isTwoElemArray $ sumEncoding options) && shouldTag -> stringE [i|[#{show constructorNameToUse}, #{interfaceName}]|]
                              | otherwise -> stringE interfaceName

    -- * Declaration
    shouldEncodeToString = null constructorFields && shouldTag
    shouldEncodeToTuple = (constructorVariant == NormalConstructor) && (not $ (isTaggedObject options && (getTagSingleConstructors options)))
    declaration = if | shouldEncodeToString -> Nothing
                     | shouldEncodeToTuple -> Just $ applyToArgsE (ConE 'TSTypeAlternatives) [stringE $ getInterfaceName constructorName,
                                                                                              ListE [stringE x | x <- genericVariables],
                                                                                              ListE [getTypeAsStringExp contentsTupleType]]
                     | otherwise -> Just $ assembleInterfaceDeclaration options constructorName genericVariables (ListE $ (tagField ++ getTSFields namesAndTypes))
                          where
                            namesAndTypes :: [(String, Type)] = case constructorVariant of
                              RecordConstructor names -> zip (fmap ((fieldLabelModifier options) . lastNameComponent') names) constructorFields
                              NormalConstructor -> case sumEncoding options of
                                TaggedObject tagFieldName contentsFieldName -> [(contentsFieldName, contentsTupleType)]
                                _ -> [(constructorNameToUse, contentsTupleType)]

                            tagField = case sumEncoding options of
                              TaggedObject tagFieldName contentsFieldName | shouldTag -> [(AppE (AppE (AppE (ConE 'TSField) (ConE 'False))
                                                                                                 (stringE tagFieldName))
                                                                                           (stringE $ [i|"#{constructorNameToUse}"|]))]
                              _ -> []

    shouldTag = (((length datatypeCons) > 1) || (getTagSingleConstructors options))
    constructorNameToUse = (constructorTagModifier options) $ lastNameComponent' constructorName
    contentsTupleType = getTupleType constructorFields


-- | Helper for handleConstructor
getTSFields :: [(String, Type)] -> [Exp]
getTSFields namesAndTypes = [(AppE (AppE (AppE (ConE 'TSField) (getOptionalAsBoolExp typ))
                                     (stringE nameString))
                               (getTypeAsStringExp typ))
                            | (nameString, typ) <- namesAndTypes]

-- | Helper for handleConstructor
assembleInterfaceDeclaration options constructorName genericVariables members = AppE (AppE (AppE (ConE 'TSInterfaceDeclaration) constructorNameExp) genericVariablesExp) members where
  constructorNameExp = stringE $ getInterfaceName constructorName
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

getInterfaceName :: Name -> String
getInterfaceName x = "I" <> (lastNameComponent' x)

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
getTagSingleConstructors options = False
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
