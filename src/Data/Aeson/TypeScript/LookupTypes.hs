{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.Aeson.TypeScript.LookupTypes where


import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson.TypeScript.Types
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Void
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

-- | Given a data type whose constructors all evaluate to string keys, and a type family,
-- build a TS interface declaration whose keys are the GADT strings and whose values are the
-- types of the value the constructor is *mapped to under the type family*.
--
-- This is useful when the data type is a GADT (so the different constructors can be tagged
-- with different types), and you want to produce a TypeScript "lookup type"
-- corresponding to the type family.
--
-- Will ignore Void values returned by the type family.
--
typeFamilyToLookupType :: Name -> Name -> Q Exp
typeFamilyToLookupType gadt typeFamily = do
  DatatypeInfo {datatypeCons} <- reifyDatatype gadt

  members <- forM (fmap constructorName datatypeCons) $ \consName -> do
    fieldName <- [|case A.toJSON $(conE consName) of A.String t -> T.unpack t; _ -> error "Constructor must serialize to string"|]
    proxyExpr <- [|(Proxy :: Proxy ($(return $ PromotedT typeFamily) $(return $ PromotedT consName)))|]
    return [|if TSType $(return proxyExpr) == TSType (Proxy :: Proxy Void) then Nothing else
             Just (TSField False $(return fieldName) (getTypeScriptType $(return proxyExpr)))|]

  let name = nameBase typeFamily <> "Lookup"

  [|TSInterfaceDeclaration $(stringE name) [] (catMaybes $(listE members))|]


-- | Given a data type like the one in 'typeFamilyToLookupType', generate a list of 'TSType'
-- representing the types of (TypeFamily Constructor), for every Constructor of the data type.
-- This is useful for gathering the TypeScript declarations for types in the image of the
-- type family.
getImageOfConstructorsUnderTypeFamily :: Name -> Name -> Q Exp
getImageOfConstructorsUnderTypeFamily gadt typeFamily = do
  DatatypeInfo {datatypeCons} <- reifyDatatype gadt

  exprs <- forM (fmap constructorName datatypeCons) $ \consName -> do
    [|TSType (Proxy :: Proxy ($(return $ PromotedT typeFamily) $(return $ PromotedT consName)))|]

  return $ ListE exprs
