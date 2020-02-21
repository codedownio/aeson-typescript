{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns, LambdaCase, PolyKinds #-}

module Data.Aeson.TypeScript.Recursive (
  getTransitiveClosure
  ) where

import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.TH
import Data.Function
import qualified Data.Set as S


getTransitiveClosure :: S.Set TSType -> S.Set TSType
getTransitiveClosure initialTypes = fix (\loop items -> let items' = S.unions (items : [getMore x | x <- S.toList items]) in
                                            if | items' == items -> items
                                               | otherwise -> loop items'
                                        ) initialTypes
  where getMore :: TSType -> S.Set TSType
        getMore (TSType x) = S.fromList $ getParentTypes x
