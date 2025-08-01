{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Data.Aeson.TypeScript.Util where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Types
import qualified Data.List as L
import Data.Proxy
import Data.String (IsString)
import Data.String.Interpolate
import qualified Data.Text as T
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype
import qualified Language.Haskell.TH.Lib as TH

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif


type Suffix = String
type Var = String

getDataTypeVars :: DatatypeInfo -> [Type]
#if MIN_VERSION_th_abstraction(0,3,0)
getDataTypeVars (DatatypeInfo {datatypeInstTypes}) = datatypeInstTypes
#else
getDataTypeVars (DatatypeInfo {datatypeVars}) = datatypeVars
#endif

coveredByDataTypeVars :: [Type] -> Type -> Bool
-- Don't include a type found in a constructor if it's already found as a datatype var
coveredByDataTypeVars dataTypeVars candidate | candidate `L.elem` dataTypeVars = True
-- Don't include a type found in a constructor if the version with a simple star kind signature is already present
coveredByDataTypeVars dataTypeVars candidate | (SigT candidate StarT) `L.elem` dataTypeVars = True
coveredByDataTypeVars _ _ = False

setDataTypeVars :: DatatypeInfo -> [Type] -> DatatypeInfo
#if MIN_VERSION_th_abstraction(0,3,0)
setDataTypeVars dti@(DatatypeInfo {}) vars = dti { datatypeInstTypes = vars }
#else
setDataTypeVars dti@(DatatypeInfo {}) vars = dti { datatypeVars = vars }
#endif

dropLeadingIFromInterfaceName :: TSDeclaration -> TSDeclaration
dropLeadingIFromInterfaceName decl@(TSInterfaceDeclaration {interfaceName=('I':xs)}) = decl { interfaceName = xs }
dropLeadingIFromInterfaceName decl@(TSTypeAlternatives {typeName=('I':xs)}) = decl { typeName = xs }
dropLeadingIFromInterfaceName x = x

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
getDatatypePredicate = AppT (ConT ''TypeScript)
#else
getDatatypePredicate typ = ClassP ''TypeScript [typ]
#endif

getTypeAsStringExp :: Type -> Q Exp
getTypeAsStringExp typ = [|getTypeScriptType (Proxy :: Proxy $(return typ))|]

getOptionalAsBoolExp :: Type -> Q Exp
getOptionalAsBoolExp typ = [|getTypeScriptOptional (Proxy :: Proxy $(return typ))|]

-- | Apply a type constructor to a list of type args
applyToArgsT :: Type -> [Type] -> Type
applyToArgsT constructor [] = constructor
applyToArgsT constructor (x:xs) = applyToArgsT (AppT constructor x) xs

-- | Apply a function to a list of args
applyToArgsE :: Exp -> [Exp] -> Exp
applyToArgsE f [] = f
applyToArgsE f (x:xs) = applyToArgsE (AppE f x) xs

-- Between Aeson 1.1.2.0 and 1.2.0.0, tagSingleConstructors was added
getTagSingleConstructors :: Options -> Bool
#if MIN_VERSION_aeson(1,2,0)
getTagSingleConstructors = tagSingleConstructors
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
  unless scopedTypeVariablesEnabled $
    error [i|The ScopedTypeVariables extension is required; please enable it before calling deriveTypeScript. (For example: put {-\# LANGUAGE ScopedTypeVariables \#-} at the top of the file.)|]
  unless (kindSignaturesEnabled || (L.null datatypeVars)) $
    error [i|The KindSignatures extension is required since type #{datatypeName} is a higher order type; please enable it before calling deriveTypeScript. (For example: put {-\# LANGUAGE KindSignatures \#-} at the top of the file.)|]
#else
assertExtensionsTurnedOn _ = return ()
#endif

-- Older versions of Aeson don't have an Eq instance for SumEncoding so we do this
isObjectWithSingleField :: SumEncoding -> Bool
isObjectWithSingleField ObjectWithSingleField = True
isObjectWithSingleField _ = False

-- Older versions of Aeson don't have an Eq instance for SumEncoding so we do this
isTwoElemArray :: SumEncoding -> Bool
isTwoElemArray TwoElemArray = True
isTwoElemArray _ = False

-- Older versions of Aeson don't have an Eq instance for SumEncoding so we do this
-- UntaggedValue was added between Aeson 0.11.3.0 and 1.0.0.0
isUntaggedValue :: SumEncoding -> Bool
#if MIN_VERSION_aeson(1,0,0)
isUntaggedValue UntaggedValue = True
#endif
isUntaggedValue _ = False

-- Between Template Haskell 2.10 and 2.11, InstanceD got an additional argument
mkInstance :: Cxt -> Type -> [Dec] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)
mkInstance = InstanceD Nothing
#else
mkInstance = InstanceD
#endif

namesAndTypes :: Options -> [(Name, (Suffix, Var))] -> ConstructorInfo -> [(Name, String, Type)]
namesAndTypes options genericVariables ci = case constructorVariant ci of
  RecordConstructor names -> zip3 names (fmap ((fieldLabelModifier options) . lastNameComponent') names) (constructorFields ci)
  _ -> case sumEncoding options of
    TaggedObject _ contentsFieldName
      | isConstructorNullary ci -> []
      | otherwise -> [(mkName "", contentsFieldName, contentsTupleTypeSubstituted genericVariables ci)]
    _ -> [(constructorName ci, constructorNameToUse options ci, contentsTupleTypeSubstituted genericVariables ci)]

constructorNameToUse :: Options -> ConstructorInfo -> String
constructorNameToUse options ci = (constructorTagModifier options) $ lastNameComponent' (constructorName ci)

-- | Get the type of a tuple of constructor fields, as when we're packing a record-less constructor into a list
contentsTupleType :: ConstructorInfo -> Type
contentsTupleType ci = let fields = constructorFields ci in
  case fields of
    [] -> AppT ListT (ConT ''())
    [x] -> x
    xs-> applyToArgsT (ConT $ tupleTypeName (L.length xs)) fields

contentsTupleTypeSubstituted :: [(Name, (Suffix, Var))] -> ConstructorInfo -> Type
contentsTupleTypeSubstituted genericVariables ci = let fields = constructorFields ci in
  case fields of
    [] -> AppT ListT (ConT ''())
    [x] -> mapType genericVariables x
    xs -> applyToArgsT (ConT $ tupleTypeName (L.length xs)) (fmap (mapType genericVariables) xs)

mapType :: [(Name, (Suffix, Var))] -> Type -> Type
mapType g x@(VarT name) = tryPromote x g name
mapType g x@(ConT name) = tryPromote x g name
mapType g x@(PromotedT name) = tryPromote x g name
mapType g (AppT typ1 typ2) = AppT (mapType g typ1) (mapType g typ2)
mapType g (SigT typ x) = SigT (mapType g typ) x
mapType g (InfixT typ1 x typ2) = InfixT (mapType g typ1) x (mapType g typ2)
mapType g (UInfixT typ1 x typ2) = UInfixT (mapType g typ1) x (mapType g typ2)
mapType g (ParensT typ) = ParensT (mapType g typ)
#if MIN_VERSION_template_haskell(2,15,0)
mapType g (AppKindT typ x) = AppKindT (mapType g typ) x
mapType g (ImplicitParamT x typ) = ImplicitParamT x (mapType g typ)
#endif
mapType _ x = x

tryPromote :: (Eq a1, Eq a2, IsString a2) => Type -> [(a1, (a3, a2))] -> a1 -> Type
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "")) = ConT ''T
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T")) = ConT ''T
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T1")) = ConT ''T1
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T2")) = ConT ''T2
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T3")) = ConT ''T3
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T4")) = ConT ''T4
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T5")) = ConT ''T5
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T6")) = ConT ''T6
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T7")) = ConT ''T7
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T8")) = ConT ''T8
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T9")) = ConT ''T9
tryPromote _ genericVariables (flip L.lookup genericVariables -> Just (_, "T10")) = ConT ''T10
tryPromote x _ _ = x

getBracketsExpression :: Bool -> [(Name, (Suffix, Var))] -> Q Exp
getBracketsExpression _ [] = [|""|]
getBracketsExpression includeSuffix names =
  [|let vars = $(genericVariablesListExpr includeSuffix names) in "<" <> L.intercalate ", " vars <> ">"|]

getBracketsExpressionAllTypesNoSuffix :: [(Name, (Suffix, Var))] -> Q Exp
getBracketsExpressionAllTypesNoSuffix [] = [|""|]
getBracketsExpressionAllTypesNoSuffix names = [|"<" <> L.intercalate ", " $(listE [ [|(getTypeScriptType (Proxy :: Proxy $(varT x)))|] | (x, (_suffix, _)) <- names]) <> ">"|]

genericVariablesListExpr :: Bool -> [(Name, (Suffix, Var))] -> Q Exp
genericVariablesListExpr includeSuffix genericVariables = listE (fmap (\((_, (suffix, _)), correspondingGeneric) ->
  [|(getTypeScriptType (Proxy :: Proxy $(return correspondingGeneric))) <> $(TH.stringE (if includeSuffix then suffix else ""))|])
  (case genericVariables of
      [x] -> [(x, ConT ''T)]
      xs -> zip xs allStarConstructors)
  )

isStarType :: Type -> Maybe Name
isStarType (SigT (VarT n) StarT) = Just n
isStarType _ = Nothing

nothingOnFail :: Q a -> Q (Maybe a)
nothingOnFail action = recover (return Nothing) (Just <$> action)

tryGetDoc :: (String -> String) -> Name -> Q Exp
tryGetDoc haddockModifier n = do
#if MIN_VERSION_template_haskell(2,18,0)
  maybeDoc <- nothingOnFail (getDoc (DeclDoc n)) >>= \case
    Just (Just doc) -> return $ Just $ Just $ haddockModifier doc
    x -> return x
#else
  let maybeDoc = Nothing
#endif

  case maybeDoc of
    Just (Just doc) -> [|Just $(TH.stringE doc)|]
    _ -> [|Nothing|]
