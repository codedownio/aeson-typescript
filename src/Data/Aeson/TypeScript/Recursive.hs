{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Data.Aeson.TypeScript.Recursive (
  -- * Getting declarations recursively
  getTransitiveClosure
  , getTypeScriptDeclarationsRecursively

  -- * Deriving missing instances recursively
  , recursivelyDeriveMissingTypeScriptInstancesFor
  , recursivelyDeriveMissingInstancesFor
  , deriveInstanceIfNecessary
  , doesTypeScriptInstanceExist
  , getAllParentTypes
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Util (nothingOnFail)
import Data.Bifunctor
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import Data.String.Interpolate
import Language.Haskell.TH as TH
import Language.Haskell.TH.Datatype


getTransitiveClosure :: S.Set TSType -> S.Set TSType
getTransitiveClosure = fix $ \loop items ->
  let items' = S.unions (items : [getMore x | x <- S.toList items])
   in if
          | items' == items -> items
          | otherwise -> loop items'

  where getMore :: TSType -> S.Set TSType
        getMore (TSType x) = S.fromList $ getParentTypes x

getTypeScriptDeclarationsRecursively :: (TypeScript a) => Proxy a -> [TSDeclaration]
getTypeScriptDeclarationsRecursively initialType = S.toList $ S.fromList declarations
  where
    closure = getTransitiveClosure (S.fromList [TSType initialType])
    declarations = mconcat [getTypeScriptDeclarations x | TSType x <- S.toList closure]


-- * Recursively deriving missing TypeScript interfaces

recursivelyDeriveMissingTypeScriptInstancesFor :: (Monoid w) => Name -> (Name -> Q w) -> Q w
recursivelyDeriveMissingTypeScriptInstancesFor = recursivelyDeriveMissingInstancesFor doesTypeScriptInstanceExist

recursivelyDeriveMissingInstancesFor :: (Monoid w) => (Name -> Q Bool) -> Name -> (Name -> Q w) -> Q w
recursivelyDeriveMissingInstancesFor doesInstanceExist name deriveFn = execWriterT $ do
  deriveInstanceIfNecessary name deriveFn

  names <- lift $ getAllParentTypes name doesInstanceExist
  forM_ names $ \n -> deriveInstanceIfNecessary n deriveFn

deriveInstanceIfNecessary :: (Monoid w) => Name -> (Name -> Q w) -> WriterT w Q ()
deriveInstanceIfNecessary name deriveFn = do
  lift (doesTypeScriptInstanceExist name) >>= \case
    True -> return ()
    False -> do
      (lift $ nothingOnFail (deriveFn name)) >>= \case
        Nothing -> lift $ reportWarning [i|Failed to derive decls for name '#{name}'|]
        Just x -> tell x

doesTypeScriptInstanceExist :: Name -> Q Bool
doesTypeScriptInstanceExist name = do
  result :: Maybe Bool <- runMaybeT $ do
    (DatatypeInfo {..}) <- MaybeT $ nothingOnFail $ reifyDatatype name

    -- Skip names with type parameters for now
    when (datatypeVars /= []) $ fail ""

    MaybeT $ nothingOnFail $ isInstance ''TypeScript [ConT name]

  return $ fromMaybe True result

getAllParentTypes :: Name -> (Name -> Q Bool) -> Q [Name]
getAllParentTypes name pruneFn = reverse <$> execStateT (getAllParentTypes' name pruneFn) []
  where
    getAllParentTypes' :: Name -> (Name -> Q Bool) -> StateT [Name] Q ()
    getAllParentTypes' nm pfn = (lift $ nothingOnFail $ pfn nm) >>= \case
      Nothing -> return ()
      Just True -> return ()
      Just False -> (lift $ nothingOnFail (reifyDatatype nm)) >>= \case
        Nothing -> do
          lift $ reportWarning [i|Failed to reify: '#{nm}'|]
        Just (DatatypeInfo {..}) -> do
          let parentTypes = mconcat $ fmap constructorFields datatypeCons

          let maybeRecurse n = do
                st <- get
                unless (n `L.elem` st) $ do
                  modify (n :)
                  getAllParentTypes' n pfn

          forM_ parentTypes $ \typ -> do
            let names :: [Name] = fst $ execState (getNamesFromType typ) ([], [typ])
            forM_ names maybeRecurse

    getNamesFromType :: Type -> State ([Name], [Type]) ()
    getNamesFromType (ConT n) = modify (first $ addIfNotPresent n)
    getNamesFromType (AppT t1 t2) = handleTwoTypes t1 t2
    getNamesFromType (InfixT t1 _ t2) = handleTwoTypes t1 t2
    getNamesFromType (UInfixT t1 _ t2) = handleTwoTypes t1 t2
    getNamesFromType _ = return ()

    handleTwoTypes t1 t2 = do
      (_, visitedTypes) <- get
      unless (t1 `L.elem` visitedTypes) $ do
        modify (second (t1 :))
        getNamesFromType t1

      (_, visitedTypes') <- get
      unless (t2 `L.elem` visitedTypes') $ do
        modify (second (t2 :))
        getNamesFromType t2

    addIfNotPresent :: (Eq a) => a -> [a] -> [a]
    addIfNotPresent x xs | x `L.elem` xs = xs
    addIfNotPresent x xs = x : xs
