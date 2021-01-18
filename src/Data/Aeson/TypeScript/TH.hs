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
deriveTypeScript :: Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                 -> Q [Dec]
deriveTypeScript options name = do
  datatypeInfo@(DatatypeInfo {..}) <- reifyDatatype name

  assertExtensionsTurnedOn datatypeInfo

  -- Build constraints: a TypeScript constraint for every constructor type and one for every type variable.
  -- Probably overkill/not exactly right, but it's a start.
  let constructorPreds :: [Pred] = [AppT (ConT ''TypeScript) x | x <- mconcat $ fmap constructorFields datatypeCons]
  let typeVariablePreds :: [Pred] = [AppT (ConT ''TypeScript) x | x <- getDataTypeVars datatypeInfo]
  let predicates = constructorPreds <> typeVariablePreds
  let constraints = foldl AppT (TupleT (length predicates)) predicates

  [d|instance $(return constraints) => TypeScript $(return $ foldl AppT (ConT name) (getDataTypeVars datatypeInfo)) where
       getTypeScriptType _ = $(TH.stringE $ getTypeName datatypeName)
       getTypeScriptDeclarations _ = $(getDeclarationFunctionBody options datatypeInfo [])
       getParentTypes _ = $(listE [ [|TSType (Proxy :: Proxy $(return t))|]
                                  | t <- mconcat $ fmap constructorFields datatypeCons])
       |]

getDeclarationFunctionBody :: Options -> DatatypeInfo -> [String] -> Q Exp
getDeclarationFunctionBody options datatypeInfo@(DatatypeInfo {..}) genericVariables = do
  (types, extraDeclsOrGenericInfos) <- runWriterT $ mapM (handleConstructor options datatypeInfo genericVariables) datatypeCons

  typeDeclaration <- [|TSTypeAlternatives $(TH.stringE $ getTypeName datatypeName)
                                          $(listE [TH.stringE x | x <- genericVariables])
                                          $(listE $ fmap return types)|]

  let extraDecls = [x | ExtraDecl x <- extraDeclsOrGenericInfos]
  let genericInfos = [(x, y) | GenericInfo x y <- extraDeclsOrGenericInfos]
  reportWarning [i|Got genericInfos: #{genericInfos}|]

  [| $(return typeDeclaration) : $(listE (fmap return $ extraDecls)) |]

data ExtraDeclOrGenericInfo = ExtraDecl Exp
                            | GenericInfo Name GenericInfoExtra

data GenericInfoExtra = NormalStar
                      | TypeFamilyKey Name
  deriving Show

-- | Return a string to go in the top-level type declaration, plus an optional expression containing a declaration
handleConstructor :: Options -> DatatypeInfo -> [String] -> ConstructorInfo -> WriterT [ExtraDeclOrGenericInfo] Q Exp
handleConstructor options (DatatypeInfo {..}) genericVariables ci@(ConstructorInfo {}) = 
  if | (length datatypeCons == 1) && not (getTagSingleConstructors options) -> do
         writeSingleConstructorEncoding
         lift $ TH.stringE interfaceNameWithBrackets

     | allConstructorsAreNullary datatypeCons && allNullaryToStringTag options -> stringEncoding

     -- With UntaggedValue, nullary constructors are encoded as strings
     | (isUntaggedValue $ sumEncoding options) && isConstructorNullary ci -> stringEncoding

     -- Treat as a sum
     | isObjectWithSingleField $ sumEncoding options -> do
         writeSingleConstructorEncoding
         lift $ TH.stringE [i|{#{show constructorNameToUse}: #{interfaceNameWithBrackets}}|]
     | isTwoElemArray $ sumEncoding options -> do
         writeSingleConstructorEncoding
         lift $ TH.stringE [i|[#{show constructorNameToUse}, #{interfaceNameWithBrackets}]|]
     | isUntaggedValue $ sumEncoding options -> do
         writeSingleConstructorEncoding
         lift $ TH.stringE interfaceNameWithBrackets
     | otherwise -> do
         tagField :: [Exp] <- lift $ case sumEncoding options of
           TaggedObject tagFieldName _ -> (: []) <$> [|TSField False $(TH.stringE tagFieldName) $(TH.stringE [i|"#{constructorNameToUse}"|])|]
           _ -> return []

         tsFields <- getTSFields
         decl <- lift $ assembleInterfaceDeclaration (ListE (tagField ++ tsFields))

         tell [ExtraDecl decl]

         lift $ TH.stringE interfaceNameWithBrackets

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
    interfaceNameWithBrackets = interfaceName <> getGenericBrackets genericVariables

    tupleEncoding = [|TSTypeAlternatives $(TH.stringE interfaceName)
                                         $(listE [TH.stringE x | x <- genericVariables])
                                         [getTypeScriptType (Proxy :: Proxy $(return contentsTupleType))]|]

    namesAndTypes :: [(String, Type)] = case constructorVariant ci of
      RecordConstructor names -> zip (fmap ((fieldLabelModifier options) . lastNameComponent') names) (constructorFields ci)
      NormalConstructor -> case sumEncoding options of
        TaggedObject _ contentsFieldName -> if | isConstructorNullary ci -> []
                                                          | otherwise -> [(contentsFieldName, contentsTupleType)]
        _ -> [(constructorNameToUse, contentsTupleType)]

    constructorNameToUse = (constructorTagModifier options) $ lastNameComponent' (constructorName ci)
    contentsTupleType = getTupleType (constructorFields ci)

    assembleInterfaceDeclaration members = [|TSInterfaceDeclaration $(TH.stringE interfaceName) $(listE [TH.stringE x | x <- genericVariables]) $(return members)|]

    getTSFields :: WriterT [ExtraDeclOrGenericInfo] Q [Exp]
    getTSFields = do
      forM namesAndTypes $ \(nameString, typ) -> do
        searchGenericInfos typ

        (fieldTyp, optAsBool) <- case typ of
          (AppT (ConT name) t)
            | name == ''Maybe && not (omitNothingFields options) -> do
                fieldTyp <- lift $ [|$(return $ getTypeAsStringExp t) <> " | null"|]
                return (fieldTyp, getOptionalAsBoolExp t)
          x -> return (getTypeAsStringExp typ, getOptionalAsBoolExp typ)
        lift $ [| TSField $(return optAsBool) nameString $(return fieldTyp) |]

    searchGenericInfos :: Type -> WriterT [ExtraDeclOrGenericInfo] Q ()
    searchGenericInfos (AppT (ConT name) typ) = lift (reify name) >>= \case
      FamilyI (ClosedTypeFamilyD (TypeFamilyHead typeFamilyName _ _ _) _) _ -> do
        lift $ reportWarning [i|Found a type family application! #{name}|]
        tell [GenericInfo (mkName "unknown") (TypeFamilyKey typeFamilyName)]
      _ -> searchGenericInfos typ
    searchGenericInfos (AppT t1 t2) = do
      searchGenericInfos t1
      searchGenericInfos t2
    searchGenericInfos (VarT name) = tell [GenericInfo name NormalStar]
    searchGenericInfos _ = return ()

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

