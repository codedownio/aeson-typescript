{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns, PolyKinds #-}

module Data.Aeson.TypeScript.Util where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Types
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype
import qualified Language.Haskell.TH.Lib as TH


#if MIN_VERSION_th_abstraction(0,3,0)
getDataTypeVars (DatatypeInfo {datatypeInstTypes}) = datatypeInstTypes
#else
getDataTypeVars (DatatypeInfo {datatypeVars}) = datatypeVars
#endif

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

namesAndTypes :: Options -> ConstructorInfo -> [(String, Type)]
namesAndTypes options ci = case constructorVariant ci of
  RecordConstructor names -> zip (fmap ((fieldLabelModifier options) . lastNameComponent') names) (constructorFields ci)
  NormalConstructor -> case sumEncoding options of
    TaggedObject _ contentsFieldName
      | isConstructorNullary ci -> []
      | otherwise -> [(contentsFieldName, contentsTupleType ci)]
    _ -> [(constructorNameToUse options ci, contentsTupleType ci)]

constructorNameToUse options ci = (constructorTagModifier options) $ lastNameComponent' (constructorName ci)

contentsTupleType ci = getTupleType (constructorFields ci)

getBracketsExpression :: [Name] -> Q Exp
getBracketsExpression [] = [|""|]
getBracketsExpression names = [|case $(genericVariablesListExpr names) of [] -> ""; vars -> "<" <> L.intercalate ", " vars <> ">"|]

getBracketsExpressionAllTypes :: [Name] -> Q Exp
getBracketsExpressionAllTypes [] = [|""|]
getBracketsExpressionAllTypes names = [|"<" <> L.intercalate ", " $(listE [ [|getTypeScriptType (Proxy :: Proxy $(varT x))|] | x <- names]) <> ">"|]

genericVariablesListExpr :: [Name] -> Q Exp
genericVariablesListExpr genericVariables = [|catMaybes $(listE (fmap (\x ->
  [|if isGenericVariable (Proxy :: Proxy $(varT x)) then Just (getTypeScriptType (Proxy :: Proxy $(varT x))) else Nothing|])
  genericVariables))|]

