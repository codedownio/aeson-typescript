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
>>> putStrLn $ 'formatTSDeclarations' $ 'getTypeScriptDeclarations' (Proxy :: Proxy (D T))

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
  deriveTypeScript
  , deriveTypeScript'
  , deriveTypeScriptLookupType

  -- * The main typeclass
  , TypeScript(..)
  , TSType(..)

  , TSDeclaration(TSRawDeclaration)

  -- * Formatting declarations
  , formatTSDeclarations
  , formatTSDeclarations'
  , formatTSDeclaration
  , FormattingOptions(..)
  , SumTypeFormat(..)
  , ExportMode(..)

  -- * Advanced options
  , ExtraTypeScriptOptions(..)

  -- * Convenience tools
  , HasJSONOptions(..)
  , deriveJSONAndTypeScript
  , deriveJSONAndTypeScript'

  , T(..)
  , T1(..)
  , T2(..)
  , T3(..)

  , module Data.Aeson.TypeScript.Instances
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
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import Data.String.Interpolate
import Data.Typeable
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype
import qualified Language.Haskell.TH.Lib as TH

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

-- | Generates a 'TypeScript' instance declaration for the given data type.
deriveTypeScript' :: Options
                  -- ^ Encoding options.
                  -> Name
                  -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                  -> ExtraTypeScriptOptions
                  -- ^ Extra options to control advanced features.
                  -> Q [Dec]
deriveTypeScript' options name extraOptions = do
  datatypeInfo' <- reifyDatatype name
  assertExtensionsTurnedOn datatypeInfo'

  -- Plug in generic variables for all star free variables
  let starVars = [name | (isStarType -> Just _) <- getDataTypeVars datatypeInfo']
  let templateVarsToUse = case length starVars of
        1 -> [ConT ''T]
        _ -> take (length starVars) allStarConstructors
  let subMap = M.fromList $ zip starVars templateVarsToUse
  let dti = datatypeInfo' { datatypeCons = fmap (applySubstitution subMap) (datatypeCons datatypeInfo')}

  -- Build constraints: a TypeScript constraint for every constructor type and one for every type variable.
  -- Probably overkill/not exactly right, but it's a start.
  let constructorPreds :: [Pred] = [AppT (ConT ''TypeScript) x | x <- mconcat $ fmap constructorFields (datatypeCons dti)
                                                               , hasFreeTypeVariable x
                                                               , not $ coveredByDataTypeVars (getDataTypeVars dti) x
                                                               ]
  let constructorPreds' :: [Pred] = [AppT (ConT ''TypeScript) x | x <- mconcat $ fmap constructorFields (datatypeCons datatypeInfo')
                                                                , hasFreeTypeVariable x
                                                                , not $ coveredByDataTypeVars (getDataTypeVars dti) x
                                                                ]
  let typeVariablePreds :: [Pred] = [AppT (ConT ''TypeScript) x | x <- getDataTypeVars dti]

  let eligibleGenericVars = catMaybes $ flip fmap (getDataTypeVars dti) $ \case
        SigT (VarT n) StarT -> Just n
        _ -> Nothing
  genericVariablesAndSuffixes <- forM eligibleGenericVars $ \var -> do
    (_, genericInfos) <- runWriterT $ forM_ (datatypeCons datatypeInfo') $ \ci ->
      forM_ (namesAndTypes options ci) $ \(_, typ) -> do
        searchForConstraints extraOptions typ var
    return (var, unifyGenericVariable genericInfos)

  -- Build the declarations
  (types, extraDeclsOrGenericInfos) <- runWriterT $ mapM (handleConstructor options extraOptions dti genericVariablesAndSuffixes) (datatypeCons dti)
  typeDeclaration <- [|TSTypeAlternatives $(TH.stringE $ getTypeName (datatypeName dti))
                                          $(genericVariablesListExpr True genericVariablesAndSuffixes)
                                          $(listE $ fmap return types)|]
  let extraDecls = [x | ExtraDecl x <- extraDeclsOrGenericInfos]
  let extraTopLevelDecls = mconcat [x | ExtraTopLevelDecs x <- extraDeclsOrGenericInfos]
  let predicates = L.nub (constructorPreds <> constructorPreds' <> typeVariablePreds <> [x | ExtraConstraint x <- extraDeclsOrGenericInfos])

  declarationsFunctionBody <- [| $(return typeDeclaration) : $(listE (fmap return $ extraDecls)) |]

  let extraParentTypes = [x | ExtraParentType x <- extraDeclsOrGenericInfos]

  -- Couldn't figure out how to put the constraints for "instance TypeScript..." in the quasiquote above without
  -- introducing () when the constraints are empty, which causes "illegal tuple constraint" unless the user enables ConstraintKinds.
  -- So, just use our mkInstance function
  getTypeScriptTypeExp <- [|$(TH.stringE $ getTypeName (datatypeName dti)) <> $(getBracketsExpressionAllTypesNoSuffix genericVariablesAndSuffixes)|]
  getParentTypesExp <- listE [ [|TSType (Proxy :: Proxy $(return t))|]
                             | t <- (mconcat $ fmap constructorFields (datatypeCons datatypeInfo')) <> extraParentTypes]
  let inst = [mkInstance predicates (AppT (ConT ''TypeScript) (foldl AppT (ConT name) (getDataTypeVars dti))) [
                 FunD 'getTypeScriptType [Clause [WildP] (NormalB getTypeScriptTypeExp) []]
                 , FunD 'getTypeScriptDeclarations [Clause [WildP] (NormalB declarationsFunctionBody) []]
                 , FunD 'getParentTypes [Clause [WildP] (NormalB getParentTypesExp) []]
                 ]]
  return (extraTopLevelDecls <> inst)

-- | Return a string to go in the top-level type declaration, plus an optional expression containing a declaration
handleConstructor :: Options -> ExtraTypeScriptOptions -> DatatypeInfo -> [(Name, String)] -> ConstructorInfo -> WriterT [ExtraDeclOrGenericInfo] Q Exp
handleConstructor options extraOptions (DatatypeInfo {..}) genericVariables ci@(ConstructorInfo {}) =
  if | (length datatypeCons == 1) && not (getTagSingleConstructors options) -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|$(TH.stringE interfaceName) <> $(return brackets)|]
     | allConstructorsAreNullary datatypeCons && allNullaryToStringTag options -> stringEncoding

     -- With UntaggedValue, nullary constructors are encoded as strings
     | (isUntaggedValue $ sumEncoding options) && isConstructorNullary ci -> stringEncoding

     -- Treat as a sum
     | isObjectWithSingleField $ sumEncoding options -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|"{" <> $(TH.stringE $ show $ constructorNameToUse options ci) <> ": " <> $(TH.stringE interfaceName) <> $(return brackets) <> "}"|]
     | isTwoElemArray $ sumEncoding options -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|"[" <> $(TH.stringE $ show $ constructorNameToUse options ci) <> ", " <> $(TH.stringE interfaceName) <> $(return brackets) <> "]"|]
     | isUntaggedValue $ sumEncoding options -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|$(TH.stringE interfaceName) <> $(return brackets)|]
     | otherwise -> do
         tagField :: [Exp] <- lift $ case sumEncoding options of
           TaggedObject tagFieldName _ -> (: []) <$> [|TSField False $(TH.stringE tagFieldName) $(TH.stringE [i|"#{constructorNameToUse options ci}"|])|]
           _ -> return []

         tsFields <- getTSFields
         decl <- lift $ assembleInterfaceDeclaration (ListE (tagField ++ tsFields))
         tell [ExtraDecl decl]
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|$(TH.stringE interfaceName) <> $(return brackets)|]

  where
    stringEncoding = lift $ TH.stringE [i|"#{(constructorTagModifier options) $ getTypeName (constructorName ci)}"|]

    writeSingleConstructorEncoding = if
      | constructorVariant ci == NormalConstructor -> do
          encoding <- tupleEncoding
          tell [ExtraDecl encoding]
      | otherwise -> do
          tsFields <- getTSFields
          decl <- lift $ assembleInterfaceDeclaration (ListE tsFields)
          tell [ExtraDecl decl]

    -- * Type declaration to use
    interfaceName = "I" <> (lastNameComponent' $ constructorName ci)

    tupleEncoding = do
      tupleType <- transformTypeFamilies extraOptions (contentsTupleType ci)
      lift $ [|TSTypeAlternatives $(TH.stringE interfaceName)
                                  $(genericVariablesListExpr True genericVariables)
                                  [getTypeScriptType (Proxy :: Proxy $(return tupleType))]|]

    assembleInterfaceDeclaration members = [|TSInterfaceDeclaration $(TH.stringE interfaceName)
                                                                    $(genericVariablesListExpr True genericVariables)
                                                                    $(return members)|]

    getTSFields :: WriterT [ExtraDeclOrGenericInfo] Q [Exp]
    getTSFields = forM (namesAndTypes options ci) $ \(nameString, typ') -> do
      typ <- transformTypeFamilies extraOptions typ'
      when (typ /= typ') $ do
        let constraint = AppT (ConT ''TypeScript) typ
        tell [ExtraConstraint constraint]

      (fieldTyp, optAsBool) <- lift $ case typ of
        (AppT (ConT name) t) | name == ''Maybe && not (omitNothingFields options) ->
          ( , ) <$> [|$(getTypeAsStringExp t) <> " | null"|] <*> getOptionalAsBoolExp t
        _ -> ( , ) <$> getTypeAsStringExp typ <*> getOptionalAsBoolExp typ'
      lift $ [| TSField $(return optAsBool) $(TH.stringE nameString) $(return fieldTyp) |]

transformTypeFamilies :: ExtraTypeScriptOptions -> Type -> WriterT [ExtraDeclOrGenericInfo] Q Type
transformTypeFamilies eo@(ExtraTypeScriptOptions {..}) (AppT (ConT name) typ)
  | name `L.elem` typeFamiliesToMapToTypeScript = lift (reify name) >>= \case
      FamilyI (ClosedTypeFamilyD (TypeFamilyHead typeFamilyName _ _ _) eqns) _ -> handle typeFamilyName eqns
      FamilyI (OpenTypeFamilyD (TypeFamilyHead typeFamilyName _ _ _)) decs -> handle typeFamilyName [eqn | TySynInstD eqn <- decs]
      _ -> AppT (ConT name) <$> transformTypeFamilies eo typ
  | otherwise = AppT (ConT name) <$> transformTypeFamilies eo typ
        where
          handle :: Name -> [TySynEqn] -> WriterT [ExtraDeclOrGenericInfo] Q Type
          handle typeFamilyName eqns = do
            name' <- lift $ newName (nameBase typeFamilyName <> "'")

            f <- lift $ newName "f"
#if MIN_VERSION_template_haskell(2,17,0)
            let inst1 = DataD [] name' [PlainTV f ()] Nothing [] []
#else
            let inst1 = DataD [] name' [PlainTV f] Nothing [] []
#endif
            tell [ExtraTopLevelDecs [inst1]]

            imageTypes <- lift $ getClosedTypeFamilyImage eqns
            inst2 <- lift $ [d|instance (Typeable g, TypeScript g) => TypeScript ($(conT name') g) where
                                 getTypeScriptType _ = $(TH.stringE $ nameBase name) <> "[" <> (getTypeScriptType (Proxy :: Proxy g)) <> "]"
                                 getTypeScriptDeclarations _ = [$(getClosedTypeFamilyInterfaceDecl name eqns)]
                                 getParentTypes _ = $(listE [ [|TSType (Proxy :: Proxy $(return x))|] | x <- imageTypes])
                            |]
            tell [ExtraTopLevelDecs inst2]

            tell [ExtraParentType (AppT (ConT name') (ConT ''T))]

            transformTypeFamilies eo (AppT (ConT name') typ)

transformTypeFamilies eo (AppT typ1 typ2) = AppT <$> transformTypeFamilies eo typ1 <*> transformTypeFamilies eo typ2
transformTypeFamilies eo (SigT typ kind) = flip SigT kind <$> transformTypeFamilies eo typ
transformTypeFamilies eo (InfixT typ1 n typ2) = InfixT <$> transformTypeFamilies eo typ1 <*> pure n <*> transformTypeFamilies eo typ2
transformTypeFamilies eo (UInfixT typ1 n typ2) = UInfixT <$> transformTypeFamilies eo typ1 <*> pure n <*> transformTypeFamilies eo typ2
transformTypeFamilies eo (ParensT typ) = ParensT <$> transformTypeFamilies eo typ
#if MIN_VERSION_template_haskell(2,15,0)
transformTypeFamilies eo (AppKindT typ kind) = flip AppKindT kind <$> transformTypeFamilies eo typ
transformTypeFamilies eo (ImplicitParamT s typ) = ImplicitParamT s <$> transformTypeFamilies eo typ
#endif
transformTypeFamilies _ typ = return typ


searchForConstraints :: ExtraTypeScriptOptions -> Type -> Name -> WriterT [GenericInfo] Q ()
searchForConstraints eo@(ExtraTypeScriptOptions {..}) (AppT (ConT name) typ) var
  | typ == VarT var && (name `L.elem` typeFamiliesToMapToTypeScript) = lift (reify name) >>= \case
      FamilyI (ClosedTypeFamilyD (TypeFamilyHead typeFamilyName _ _ _) _) _ -> do
        tell [GenericInfo var (TypeFamilyKey typeFamilyName)]
        searchForConstraints eo typ var
      FamilyI (OpenTypeFamilyD (TypeFamilyHead typeFamilyName _ _ _)) _ -> do
        tell [GenericInfo var (TypeFamilyKey typeFamilyName)]
        searchForConstraints eo typ var
      _ -> searchForConstraints eo typ var
  | otherwise = searchForConstraints eo typ var
searchForConstraints eo (AppT typ1 typ2) var = searchForConstraints eo typ1 var >> searchForConstraints eo typ2 var
searchForConstraints eo (SigT typ _) var = searchForConstraints eo typ var
searchForConstraints eo (InfixT typ1 _ typ2) var = searchForConstraints eo typ1 var >> searchForConstraints eo typ2 var
searchForConstraints eo (UInfixT typ1 _ typ2) var = searchForConstraints eo typ1 var >> searchForConstraints eo typ2 var
searchForConstraints eo (ParensT typ) var = searchForConstraints eo typ var
#if MIN_VERSION_template_haskell(2,15,0)
searchForConstraints eo (AppKindT typ _) var = searchForConstraints eo typ var
searchForConstraints eo (ImplicitParamT _ typ) var = searchForConstraints eo typ var
#endif
searchForConstraints _ _ _ = return ()

hasFreeTypeVariable :: Type -> Bool
hasFreeTypeVariable (VarT _) = True
hasFreeTypeVariable (AppT typ1 typ2) = hasFreeTypeVariable typ1 || hasFreeTypeVariable typ2
hasFreeTypeVariable (SigT typ _) = hasFreeTypeVariable typ
hasFreeTypeVariable (InfixT typ1 _ typ2) = hasFreeTypeVariable typ1 || hasFreeTypeVariable typ2
hasFreeTypeVariable (UInfixT typ1 _ typ2) = hasFreeTypeVariable typ1 || hasFreeTypeVariable typ2
hasFreeTypeVariable (ParensT typ) = hasFreeTypeVariable typ
#if MIN_VERSION_template_haskell(2,15,0)
hasFreeTypeVariable (AppKindT typ _) = hasFreeTypeVariable typ
hasFreeTypeVariable (ImplicitParamT _ typ) = hasFreeTypeVariable typ
#endif
hasFreeTypeVariable _ = False

unifyGenericVariable :: [GenericInfo] -> String
unifyGenericVariable genericInfos = case [nameBase name | GenericInfo _ (TypeFamilyKey name) <- genericInfos] of
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

deriveJSONAndTypeScript' :: Options
                         -- ^ Encoding options.
                         -> Name
                         -- ^ Name of the type for which to generate 'A.ToJSON', 'A.FromJSON', and 'TypeScript' instance declarations.
                         -> ExtraTypeScriptOptions
                         -- ^ Extra options to control advanced features.
                         -> Q [Dec]
deriveJSONAndTypeScript' options name extraOptions = (<>) <$> (deriveTypeScript' options name extraOptions) <*> (A.deriveJSON options name)

-- | Generates a 'TypeScript' instance declaration for the given data type.
deriveTypeScript :: Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                 -> Q [Dec]
deriveTypeScript options name = deriveTypeScript' options name defaultExtraTypeScriptOptions
