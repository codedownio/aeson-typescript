{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}

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
  deriveTypeScript',
  deriveTypeScriptLookupType,

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
  T1(..),
  T2(..),
  T3(..),
    
  module Data.Aeson.TypeScript.Instances
  ) where

import Control.Monad
import Control.Monad.Writer
import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.Formatting
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Lookup
import Data.Aeson.TypeScript.Types
import Data.Aeson.TypeScript.Util
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype
import qualified Language.Haskell.TH.Lib as TH


-- | Generates a 'TypeScript' instance declaration for the given data type.
deriveTypeScript' :: Options
                  -- ^ Encoding options.
                  -> Name
                  -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                  -> ExtraTypeScriptOptions
                  -- ^ Extra options to control advanced features.
                  -> Q [Dec]
deriveTypeScript' options name extraOptions = do
  datatypeInfo@(DatatypeInfo {..}) <- reifyDatatype name

  assertExtensionsTurnedOn datatypeInfo

  -- Build constraints: a TypeScript constraint for every constructor type and one for every type variable.
  -- Probably overkill/not exactly right, but it's a start.
  let constructorPreds :: [Pred] = [AppT (ConT ''TypeScript) x | x <- mconcat $ fmap constructorFields datatypeCons]
  let typeVariablePreds :: [Pred] = [AppT (ConT ''TypeScript) x | x <- getDataTypeVars datatypeInfo]
  let predicates = constructorPreds <> typeVariablePreds
  let constraints = foldl AppT (TupleT (length predicates)) predicates

  let eligibleGenericVars = catMaybes $ flip fmap (getDataTypeVars datatypeInfo) $ \case
        SigT (VarT n) StarT -> Just n
        _ -> Nothing
  genericVariablesAndSuffixes <- forM (zip eligibleGenericVars allStarConstructors') $ \tuple@(var, genericVar) -> do
    (_, genericInfos) <- runWriterT $ forM_ datatypeCons $ \ci ->
      forM_ (namesAndTypes options ci) $ \(_, typ) -> do
        searchForConstraints extraOptions typ tuple
    return (var, unifyGenericVariable genericInfos)
  let genericVariables = fmap fst genericVariablesAndSuffixes

  -- Build the declarations
  (types, extraDeclsOrGenericInfos) <- runWriterT $ mapM (handleConstructor options datatypeInfo genericVariables) datatypeCons
  typeDeclaration <- [|TSTypeAlternatives $(TH.stringE $ getTypeName datatypeName)
                                          $(genericVariablesListExpr genericVariables)
                                          $(listE $ fmap return types)|]
  let extraDecls = [x | ExtraDecl x <- extraDeclsOrGenericInfos]
  declarationsFunctionBody <- [| $(return typeDeclaration) : $(listE (fmap return $ extraDecls)) |]

  [d|instance $(return constraints) => TypeScript $(return $ foldl AppT (ConT name) (getDataTypeVars datatypeInfo)) where
       getTypeScriptType _ = $(TH.stringE $ getTypeName datatypeName) <> $(getBracketsExpressionAllTypes genericVariables)
       getTypeScriptDeclarations _ = $(return declarationsFunctionBody)
       getParentTypes _ = $(listE [ [|TSType (Proxy :: Proxy $(return t))|]
                                  | t <- mconcat $ fmap constructorFields datatypeCons])
       |]


-- | Return a string to go in the top-level type declaration, plus an optional expression containing a declaration
handleConstructor :: Options -> DatatypeInfo -> [Name] -> ConstructorInfo -> WriterT [ExtraDeclOrGenericInfo] Q Exp
handleConstructor options (DatatypeInfo {..}) genericVariables ci@(ConstructorInfo {}) = 
  if | (length datatypeCons == 1) && not (getTagSingleConstructors options) -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression genericVariables
         lift $ [|$(TH.stringE interfaceName) <> $(return brackets)|]
     | allConstructorsAreNullary datatypeCons && allNullaryToStringTag options -> stringEncoding

     -- With UntaggedValue, nullary constructors are encoded as strings
     | (isUntaggedValue $ sumEncoding options) && isConstructorNullary ci -> stringEncoding

     -- Treat as a sum
     | isObjectWithSingleField $ sumEncoding options -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression genericVariables
         lift $ [|"{" <> $(TH.stringE $ show $ constructorNameToUse options ci) <> ": " <> $(TH.stringE interfaceName) <> $(return brackets) <> "}"|]
     | isTwoElemArray $ sumEncoding options -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression genericVariables
         lift $ [|"[" <> $(TH.stringE $ show $ constructorNameToUse options ci) <> ", " <> $(TH.stringE interfaceName) <> $(return brackets) <> "]"|]
     | isUntaggedValue $ sumEncoding options -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression genericVariables
         lift $ [|$(TH.stringE interfaceName) <> $(return brackets)|]
     | otherwise -> do
         tagField :: [Exp] <- lift $ case sumEncoding options of
           TaggedObject tagFieldName _ -> (: []) <$> [|TSField False $(TH.stringE tagFieldName) $(TH.stringE [i|"#{constructorNameToUse options ci}"|])|]
           _ -> return []

         tsFields <- getTSFields
         decl <- lift $ assembleInterfaceDeclaration (ListE (tagField ++ tsFields))
         tell [ExtraDecl decl]
         brackets <- lift $ getBracketsExpression genericVariables
         lift $ [|$(TH.stringE interfaceName) <> $(return brackets)|]

  where
    stringEncoding = lift $ TH.stringE [i|"#{(constructorTagModifier options) $ getTypeName (constructorName ci)}"|]

    writeSingleConstructorEncoding = if
      | constructorVariant ci == NormalConstructor -> do
          encoding <- lift tupleEncoding
          tell [ExtraDecl encoding]
      | otherwise -> do
          tsFields <- getTSFields
          decl <- lift $ assembleInterfaceDeclaration (ListE tsFields)
          tell [ExtraDecl decl]

    -- * Type declaration to use
    interfaceName = "I" <> (lastNameComponent' $ constructorName ci)

    tupleEncoding = [|TSTypeAlternatives $(TH.stringE interfaceName)
                                         $(genericVariablesListExpr genericVariables)
                                         [getTypeScriptType (Proxy :: Proxy $(return $ contentsTupleType ci))]|]

    assembleInterfaceDeclaration members = [|TSInterfaceDeclaration $(TH.stringE interfaceName)
                                                                    $(genericVariablesListExpr genericVariables)
                                                                    $(return members)|]

    getTSFields :: WriterT [ExtraDeclOrGenericInfo] Q [Exp]
    getTSFields = do
      forM (namesAndTypes options ci) $ \(nameString, typ) -> do
        (fieldTyp, optAsBool) <- case typ of
          (AppT (ConT name) t)
            | name == ''Maybe && not (omitNothingFields options) -> do
                fieldTyp <- lift $ [|$(return $ getTypeAsStringExp t) <> " | null"|]
                return (fieldTyp, getOptionalAsBoolExp t)
          x -> return (getTypeAsStringExp typ, getOptionalAsBoolExp typ)
        lift $ [| TSField $(return optAsBool) $(TH.stringE nameString) $(return fieldTyp) |]

searchForConstraints :: ExtraTypeScriptOptions -> Type -> (Name, Name) -> WriterT [GenericInfo] Q ()
searchForConstraints eo@(ExtraTypeScriptOptions {..}) (AppT (ConT name) typ) tup@(varName, genericName) 
  | typ == VarT varName && (name `L.elem` typeFamiliesToMapToTypeScript) = lift (reify name) >>= \case
      FamilyI (ClosedTypeFamilyD (TypeFamilyHead typeFamilyName _ _ _) _) _ -> do
        tell [GenericInfo varName genericName (TypeFamilyKey typeFamilyName)]
        searchForConstraints eo typ tup
      _ -> searchForConstraints eo typ tup
  | otherwise = searchForConstraints eo typ tup
searchForConstraints eo (AppT typ1 typ2) tup = searchForConstraints eo typ1 tup >> searchForConstraints eo typ2 tup
searchForConstraints eo (AppKindT typ _) tup = searchForConstraints eo typ tup
searchForConstraints eo (SigT typ _) tup = searchForConstraints eo typ tup
searchForConstraints eo (InfixT typ1 _ typ2) tup = searchForConstraints eo typ1 tup >> searchForConstraints eo typ2 tup
searchForConstraints eo (UInfixT typ1 _ typ2) tup = searchForConstraints eo typ1 tup >> searchForConstraints eo typ2 tup
searchForConstraints eo (ParensT typ) tup = searchForConstraints eo typ tup
searchForConstraints eo (ImplicitParamT _ typ) tup = searchForConstraints eo typ tup
searchForConstraints eo _ _ = return ()

unifyGenericVariable :: [GenericInfo] -> String
unifyGenericVariable genericInfos = case [nameBase name | GenericInfo _ _ (TypeFamilyKey name) <- genericInfos] of
  [] -> ""
  names -> " extends keyof " <> (L.intercalate " & " names)

-- * Convenience functions

-- | Convenience function to generate 'A.ToJSON', 'A.FromJSON', and 'TypeScript' instances simultaneously, so the instances are guaranteed to be in sync.
--
-- This function is given mainly as an illustration.
-- If you want some other permutation of instances, such as 'A.ToJSON' and 'A.TypeScript' only, just take a look at the source and write your own version.
--
-- @since 0.1.0.4
deriveJSONAndTypeScript :: Options
                        -- ^ Encoding options.
                        -> Name
                        -- ^ Name of the type for which to generate 'A.ToJSON', 'A.FromJSON', and 'TypeScript' instance declarations.
                        -> Q [Dec]
deriveJSONAndTypeScript options name = (<>) <$> (deriveTypeScript options name) <*> (A.deriveJSON options name)


-- | Generates a 'TypeScript' instance declaration for the given data type.
deriveTypeScript :: Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                 -> Q [Dec]
deriveTypeScript options name = deriveTypeScript' options name defaultExtraTypeScriptOptions
