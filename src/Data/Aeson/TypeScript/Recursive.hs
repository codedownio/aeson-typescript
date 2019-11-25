{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, ExistentialQuantification, FlexibleInstances, NamedFieldPuns, MultiWayIf, ViewPatterns, LambdaCase, PolyKinds #-}

module Data.Aeson.TypeScript.Recursive (
  deriveTypeScriptRecursively
  , getDeclarationsRecursively

  , getTransitiveClosure
  ) where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.TH
import Data.Function
import qualified Data.List as L
import qualified Data.Set as S
import Data.String.Interpolate.IsString
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype

-- | Generates a 'TypeScript' instance declaration for the given data type,
-- and recursively for all types it depends on.
deriveTypeScriptRecursively :: Name
                            -- ^ Name of the type for which to generate 'TypeScript' instance declaration.
                            -> Maybe A.Options
                            -- ^ Aeson 'Options' to use when a 'HasJSONOptions' instance doesn't exist.
                            -> Q [Dec]
deriveTypeScriptRecursively name defaultAesonOptions = do
  (DatatypeInfo {..}) <- reifyDatatype name

  let parentTypes = mconcat $ fmap constructorFields $ datatypeCons

  parentDecls <- forM parentTypes $ \typ -> do
    isInstance ''TypeScript [typ] >>= \case
      True -> return [] -- Skip since instance already exists
      False -> findNameForType typ >>= \case
        Just n -> do
          parentInfo@(DatatypeInfo {..}) <- reifyDatatype n

          optionsToUse <- getAesonOptions defaultAesonOptions typ
          deriveTypeScript optionsToUse n
        Nothing -> do
          reportWarning [i|Couldn't look up name for '#{typ}'|]
          return []

  -- optionsToUse <- getAesonOptions defaultAesonOptions ()
  ownDecl <- deriveTypeScript A.defaultOptions name

  return ((mconcat parentDecls) <> ownDecl)

getAesonOptions :: Maybe A.Options -> Type -> Q A.Options
getAesonOptions defaultAesonOptions typ = isInstance ''HasJSONOptions [typ] >>= \case
  False -> case defaultAesonOptions of
    Nothing -> fail [i|Instance HasJSONOptions '#{typ}' didn't exist and no default options were provided|]
    Just options -> return options
  True -> do
    -- let options = getJSONOptions (Proxy :: Proxy typ)
    return A.defaultOptions

findNameForType :: Type -> Q (Maybe Name)
findNameForType (ConT name) = return $ Just name
findNameForType _ = return Nothing

getDeclarationsRecursively :: [TSType] -> [TSDeclaration] -> [TSDeclaration]
getDeclarationsRecursively [] declarationsSoFar = declarationsSoFar
getDeclarationsRecursively ((TSType x):xs) declarationsSoFar = case (getTypeScriptDeclarations x) L.\\ declarationsSoFar of
  [] -> getDeclarationsRecursively xs declarationsSoFar
  newDecls -> getDeclarationsRecursively (getParentTypes x <> xs) (newDecls <> declarationsSoFar)


getTransitiveClosure :: S.Set TSType -> S.Set TSType
getTransitiveClosure initialTypes = fix (\loop items -> let items' = S.unions (items : [getMore x | x <- S.toList items]) in
                                            if | items' == items -> items
                                               | otherwise -> loop items'
                                        ) initialTypes
  where getMore :: TSType -> S.Set TSType
        getMore (TSType x) = S.fromList $ getParentTypes x
