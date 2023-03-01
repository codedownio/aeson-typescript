{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Data.Aeson.TypeScript.TypeManipulation (
  searchForConstraints
  , hasFreeTypeVariable
  , unifyGenericVariable
  ) where

import Control.Monad.Writer
import Data.Aeson.TypeScript.Types
import qualified Data.List as L
import Language.Haskell.TH

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif


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
