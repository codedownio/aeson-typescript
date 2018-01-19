{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns #-}

module Data.Aeson.TypeScript.TypeReplacement where

import Data.Aeson as A
import Data.Aeson.TypeScript.Formatting
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Types
import Data.List (inits, tails)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype

-- | This is stupid, but I can't find a way to take a Name (of a higher-order type) and then apply it to additional type variables and get the result
-- back as a Name, rather than a type. If this were possible we could just reify that name.
-- Instead, we take the reified constructor info from the original name and traverse it, manually replacing type variables.

replaceTypesInConstructor typeReplacementMap info@(ConstructorInfo {constructorFields}) = info {constructorFields = fmap (replaceTypes typeReplacementMap) constructorFields}

replaceTypes :: M.Map Type Type -> Type -> Type
replaceTypes map candidate | M.member candidate map = fromJust $ M.lookup candidate map
replaceTypes map (AppT typ1 typ2) = AppT (replaceTypes map typ1) (replaceTypes map typ2)
replaceTypes map (SigT typ kind) = SigT (replaceTypes map typ) kind
replaceTypes map (InfixT typ1 name typ2) = InfixT (replaceTypes map typ1) name (replaceTypes map typ2)
replaceTypes map (UInfixT typ1 name typ2) = UInfixT (replaceTypes map typ1) name (replaceTypes map typ2)
replaceTypes map (ParensT typ) = ParensT (replaceTypes map typ)
replaceTypes map candidate = candidate
