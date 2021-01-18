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
import Data.Monoid
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
    FamilyI (ClosedTypeFamilyD (TypeFamilyHead name vars sig maybeInject) eqns) decs -> do
      fields <- forM eqns $ \case
        TySynEqn Nothing (AppT (ConT _) (ConT arg)) (ConT result) ->
          [| TSField False (getTypeScriptType (Proxy :: Proxy $(conT arg))) (getTypeScriptType (Proxy :: Proxy $(conT result))) |]
        x -> fail [i|Don't know how to handle type family equation: '#{x}'|]

      expr <- [| TSInterfaceDeclaration $(TH.stringE $ nameBase name) [] $(listE $ fmap return fields) |]

      return [FunD (mkName declNameStr) [Clause [] (NormalB (ListE [expr])) []]]

    _ -> fail [i|Expected a close type family; got #{info}|]
