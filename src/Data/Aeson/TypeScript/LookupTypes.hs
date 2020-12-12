{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.Aeson.TypeScript.LookupTypes where


import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson.TypeScript.Types
import Data.Proxy
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Datatype


typeFamilyToLookupType :: Name -> Name -> Q Exp
typeFamilyToLookupType gadt typeFamily = do
  DatatypeInfo {datatypeCons} <- reifyDatatype gadt

  members <- forM (fmap constructorName datatypeCons) $ \consName -> do
    fieldName <- [|case A.toJSON $(conE consName) of A.String t -> T.unpack t; _ -> error "Constructor must serialize to string"|]
    consType <- [|getTypeScriptType (Proxy :: Proxy ($(return $ PromotedT typeFamily) $(return $ PromotedT consName)))|]
    return [|TSField False $(return fieldName) $(return consType)|]

  let name = nameBase typeFamily <> "Lookup"

  [|TSInterfaceDeclaration $(stringE name) [] $(listE members)|]


getImageOfGADTUnderTypeFamily :: Name -> Name -> Q Exp
getImageOfGADTUnderTypeFamily gadt typeFamily = do
  DatatypeInfo {datatypeCons} <- reifyDatatype gadt

  exprs <- forM (fmap constructorName datatypeCons) $ \consName -> do
    [|TSType (Proxy :: Proxy ($(return $ PromotedT typeFamily) $(return $ PromotedT consName)))|]

  return $ ListE exprs
