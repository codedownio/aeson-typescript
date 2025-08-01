{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Aeson.TypeScript.Instances.TupleGen where

import Data.Aeson.TypeScript.Types
import Data.Data
import Data.List (intercalate)
import qualified Data.List as L
import Language.Haskell.TH


mkTupleInstance :: Int -> Q Dec
mkTupleInstance n = do
  let typeVars = take n $ map (mkName . (:[])) ['a'..]
      constraints = map (\tv -> AppT (ConT ''TypeScript) (VarT tv)) typeVars
      tupleType = foldl AppT (TupleT n) (map VarT typeVars)
      instanceHead = AppT (ConT ''TypeScript) tupleType

  getTypeBody <- buildTypeBody typeVars
  let getTypeMethod = FunD 'getTypeScriptType [Clause [WildP] (NormalB getTypeBody) []]

  let tsTypes = map (\tv -> AppE (ConE 'TSType) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (VarT tv)))) typeVars
      getParentsMethod = FunD 'getParentTypes [Clause [WildP] (NormalB (AppE (VarE 'L.nub) (ListE tsTypes))) []]

  return $ InstanceD Nothing constraints instanceHead [getTypeMethod, getParentsMethod]

buildTypeBody :: [Name] -> Q Exp
buildTypeBody typeVars = do
  let calls = map (\tv -> AppE (VarE 'getTypeScriptTypeOrOptionalNull)
                              (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (VarT tv)))) typeVars
      parts = [LitE (StringL "[")] ++ intercalate [LitE (StringL ", ")] (map (:[]) calls) ++ [LitE (StringL "]")]
  return $ foldr1 (\a b -> InfixE (Just a) (VarE '(++)) (Just b)) parts

mkTupleInstances :: Int -> Q [Dec]
mkTupleInstances maxArity = mapM mkTupleInstance [2..maxArity]
