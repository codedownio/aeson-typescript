{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns, PolyKinds #-}

module Data.Aeson.TypeScript.Util where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Types
import qualified Data.List as L
import Data.Proxy
import Data.String.Interpolate
import qualified Data.Text as T
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype
import qualified Language.Haskell.TH.Lib as TH

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

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
getDatatypePredicate typ = AppT (ConT ''TypeScript) typ
#else
getDatatypePredicate typ = ClassP ''TypeScript [typ]
#endif

getTypeAsStringExp :: Type -> Q Exp
getTypeAsStringExp typ = [|getTypeScriptType (Proxy :: Proxy $(return typ))|]

getOptionalAsBoolExp :: Type -> Q Exp
getOptionalAsBoolExp typ = [|getTypeScriptOptional (Proxy :: Proxy $(return typ))|]

-- | Helper to apply a type constructor to a list of type args
applyToArgsT :: Type -> [Type] -> Type
applyToArgsT constructor [] = constructor
applyToArgsT constructor (x:xs) = applyToArgsT (AppT constructor x) xs

-- | Helper to apply a function a list of args
applyToArgsE :: Exp -> [Exp] -> Exp
applyToArgsE f [] = f
applyToArgsE f (x:xs) = applyToArgsE (AppE f x) xs

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
  when (not scopedTypeVariablesEnabled) $ error [i|The ScopedTypeVariables extension is required; please enable it before calling deriveTypeScript. (For example: put {-\# LANGUAGE ScopedTypeVariables \#-} at the top of the file.)|]
  when ((not kindSignaturesEnabled) && (length datatypeVars > 0)) $ error [i|The KindSignatures extension is required since type #{datatypeName} is a higher order type; please enable it before calling deriveTypeScript. (For example: put {-\# LANGUAGE KindSignatures \#-} at the top of the file.)|]
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
mkInstance context typ decs = InstanceD Nothing context typ decs
#else
mkInstance context typ decs = InstanceD context typ decs
#endif

namesAndTypes :: Options -> ConstructorInfo -> [(String, Type)]
namesAndTypes options ci = case constructorVariant ci of
  RecordConstructor names -> zip (fmap ((fieldLabelModifier options) . lastNameComponent') names) (constructorFields ci)
  _ -> case sumEncoding options of
    TaggedObject _ contentsFieldName
      | isConstructorNullary ci -> []
      | otherwise -> [(contentsFieldName, contentsTupleType ci)]
    _ -> [(constructorNameToUse options ci, contentsTupleType ci)]

constructorNameToUse :: Options -> ConstructorInfo -> String
constructorNameToUse options ci = (constructorTagModifier options) $ lastNameComponent' (constructorName ci)

-- | Get the type of a tuple of constructor fields, as when we're packing a record-less constructor into a list
contentsTupleType :: ConstructorInfo -> Type
contentsTupleType ci = let fields = constructorFields ci in
  case length fields of
    0 -> AppT ListT (ConT ''())
    1 -> head fields
    x -> applyToArgsT (ConT $ tupleTypeName x) fields

getBracketsExpression :: Bool -> [(Name, String)] -> Q Exp
getBracketsExpression _ [] = [|""|]
getBracketsExpression includeSuffix names =
  [|let vars = $(genericVariablesListExpr includeSuffix names) in "<" <> L.intercalate ", " vars <> ">"|]

getBracketsExpressionAllTypesNoSuffix :: [(Name, String)] -> Q Exp
getBracketsExpressionAllTypesNoSuffix [] = [|""|]
getBracketsExpressionAllTypesNoSuffix names = [|"<" <> L.intercalate ", " $(listE [ [|(getTypeScriptType (Proxy :: Proxy $(varT x)))|] | (x, _suffix) <- names]) <> ">"|]

genericVariablesListExpr :: Bool -> [(Name, String)] -> Q Exp
genericVariablesListExpr includeSuffix genericVariables = listE (fmap (\((_, suffix), correspondingGeneric) ->
  [|(getTypeScriptType (Proxy :: Proxy $(return correspondingGeneric))) <> $(TH.stringE (if includeSuffix then suffix else ""))|])
  (case genericVariables of
      [x] -> [(x, ConT ''T)]
      xs -> zip xs allStarConstructors)
  )

isStarType :: Type -> Maybe Name
isStarType (SigT (VarT n) StarT) = Just n
isStarType _ = Nothing
