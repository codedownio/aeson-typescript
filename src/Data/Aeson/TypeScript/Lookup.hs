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

module Data.Aeson.TypeScript.Lookup where

import Control.Monad
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Types
import Data.Proxy
import Data.String.Interpolate.IsString
import Language.Haskell.TH hiding (stringE)
import qualified Language.Haskell.TH.Lib as TH


-- | Generates a 'TypeScript' declaration for a closed type family as a lookup type.
deriveTypeScriptLookupType :: Name
                           -- ^ Name of a type family.
                           -> String
                           -- ^ Name of the declaration to derive.
                           -> Q [Dec]
deriveTypeScriptLookupType name declNameStr = do
  info <- reify name
  case info of
    FamilyI (ClosedTypeFamilyD (TypeFamilyHead _name _vars _sig _maybeInject) eqns) _decs -> do
      interfaceDecl <- getClosedTypeFamilyInterfaceDecl name eqns
      return [FunD (mkName declNameStr) [Clause [] (NormalB (ListE [interfaceDecl])) []]]

    _ -> fail [i|Expected a close type family; got #{info}|]

getClosedTypeFamilyInterfaceDecl :: Name -> [TySynEqn] -> Q Exp
getClosedTypeFamilyInterfaceDecl name eqns = do
  fields <- forM eqns $ \case
#if MIN_VERSION_template_haskell(2,15,0)
    TySynEqn Nothing (AppT (ConT _) (ConT arg)) result -> do
#else
    TySynEqn [ConT arg] result -> do
#endif
      [| TSField False (getTypeScriptType (Proxy :: Proxy $(conT arg))) (getTypeScriptType (Proxy :: Proxy $(return result))) |]
    x -> fail [i|aeson-typescript doesn't know yet how to handle this type family equation: '#{x}'|]

  [| TSInterfaceDeclaration $(TH.stringE $ nameBase name) [] $(listE $ fmap return fields) |]

getClosedTypeFamilyImage :: [TySynEqn] -> Q [Type]
getClosedTypeFamilyImage eqns = do
  forM eqns $ \case
#if MIN_VERSION_template_haskell(2,15,0)
    TySynEqn Nothing (AppT (ConT _) (ConT _)) result -> return result
#else
    TySynEqn [ConT _] result -> return result
#endif
    x -> fail [i|aeson-typescript doesn't know yet how to handle this type family equation: '#{x}'|]
