{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns, LambdaCase, PolyKinds #-}

module Data.Aeson.TypeScript.Recursive (
  getTransitiveClosure
  , getTypeScriptDeclarationsRecursively
  ) where

import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Proxy
import qualified Data.Set as S


getTransitiveClosure :: S.Set TSType -> S.Set TSType
getTransitiveClosure initialTypes = fix (\loop items -> let items' = S.unions (items : [getMore x | x <- S.toList items]) in
                                            if | items' == items -> items
                                               | otherwise -> loop items'
                                        ) initialTypes
  where getMore :: TSType -> S.Set TSType
        getMore (TSType x) = S.fromList $ getParentTypes x

getTypeScriptDeclarationsRecursively :: (TypeScript a) => Proxy a -> [TSDeclaration]
getTypeScriptDeclarationsRecursively initialType = S.toList $ S.fromList declarations
  where
    closure = getTransitiveClosure (S.fromList [TSType initialType])
    declarations = mconcat [getTypeScriptDeclarations x | TSType x <- S.toList closure]
