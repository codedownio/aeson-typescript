{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}


module Data.Aeson.TypeScript.Transform (
  transformTypeFamilies
  ) where

import Control.Monad (unless)
import Control.Monad.Writer
import Data.Aeson.TypeScript.Lookup
import Data.Aeson.TypeScript.Types
import Data.Aeson.TypeScript.Util (splitAppsT, typeFamilyTableName)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Typeable
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype (applySubstitution)
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Syntax (getQ, putQ)

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif


-- | Tables emitted so far in this module, so that repeated use sites of one family agree on
-- a single declaration. 'Q' state persists across splices within a module.
newtype EmittedFamilyTables = EmittedFamilyTables (S.Set String)

-- | Search the given type for type families. For each one found, emit a declaration for a new
-- corresponding concrete type and a TypeScript instance for it which emits a lookup type.
-- Then, replace all occurrences of the given type family with the concrete type in the return value.
-- Thus the type becomes "de-family-ified".
transformTypeFamilies :: ExtraTypeScriptOptions -> Type -> WriterT [ExtraDeclOrGenericInfo] Q Type
transformTypeFamilies eo@(ExtraTypeScriptOptions {..}) wholeType
  | (ConT name, keyArg : trailing) <- splitAppsT wholeType
  , name `L.elem` typeFamiliesToMapToTypeScript = lift (reify name) >>= \case
      FamilyI (ClosedTypeFamilyD (TypeFamilyHead typeFamilyName _ _ _) eqns) _ -> handle name keyArg trailing typeFamilyName eqns

#if MIN_VERSION_template_haskell(2,15,0)
      FamilyI (OpenTypeFamilyD (TypeFamilyHead typeFamilyName _ _ _)) decs -> handle name keyArg trailing typeFamilyName [eqn | TySynInstD eqn <- decs]
#else
      FamilyI (OpenTypeFamilyD (TypeFamilyHead typeFamilyName _ _ _)) decs -> handle name keyArg trailing typeFamilyName [eqn | TySynInstD _name eqn <- decs]
#endif

      _ -> descend wholeType
  where
          descend (AppT typ1 typ2) = AppT <$> transformTypeFamilies eo typ1 <*> transformTypeFamilies eo typ2
          descend (SigT typ kind) = flip SigT kind <$> transformTypeFamilies eo typ
          descend (ParensT typ) = ParensT <$> transformTypeFamilies eo typ
          descend typ = return typ

          handle :: Name -> Type -> [Type] -> Name -> [TySynEqn] -> WriterT [ExtraDeclOrGenericInfo] Q Type
          handle _name keyArg trailing typeFamilyName eqnsRaw = do
            eqns <- lift $ mapM (substituteTrailingArgs trailing) eqnsRaw
            let tableName = typeFamilyTableName typeFamilyName trailing
            -- Deterministic and named after the table, so that repeated use sites of one
            -- family agree on it rather than each declaring their own.
            let name' = mkName (tableName <> "'")
            alreadyDeclared <- lift $ do
              inScope <- isJust <$> lookupTypeName (tableName <> "'")
              EmittedFamilyTables emitted <- fromMaybe (EmittedFamilyTables mempty) <$> getQ
              case inScope || S.member tableName emitted of
                True -> return True
                False -> do
                  putQ (EmittedFamilyTables (S.insert tableName emitted))
                  return False

            f <- lift $ newName "f"
#if MIN_VERSION_template_haskell(2,21,0)
            let inst1 = DataD [] name' [PlainTV f BndrReq] Nothing [] []
#elif MIN_VERSION_template_haskell(2,17,0)
            let inst1 = DataD [] name' [PlainTV f ()] Nothing [] []
#else
            let inst1 = DataD [] name' [PlainTV f] Nothing [] []
#endif
            unless alreadyDeclared $ tell [ExtraTopLevelDecs [inst1]]

            imageTypes <- lift $ getClosedTypeFamilyImage eqns
            inst2 <- lift $ [d|instance (Typeable g, TypeScript g) => TypeScript ($(conT name') g) where
                                 getTypeScriptType _ = $(TH.stringE tableName) <> "[" <> (getTypeScriptType (Proxy :: Proxy g)) <> "]"
                                 getTypeScriptDeclarations _ = [$(getClosedTypeFamilyInterfaceDecl' tableName eqns)]
                                 getParentTypes _ = $(listE [ [|TSType (Proxy :: Proxy $(return x))|] | x <- imageTypes])
                            |]
            unless alreadyDeclared $ tell [ExtraTopLevelDecs inst2]

            tell [ExtraParentType (AppT (ConT name') (ConT ''T))]

            ret <- transformTypeFamilies eo (AppT (ConT name') keyArg)
            tell [ExtraConstraint (AppT (ConT ''TypeScript) ret)]
            return ret
transformTypeFamilies eo (AppT typ1 typ2) = AppT <$> transformTypeFamilies eo typ1 <*> transformTypeFamilies eo typ2
transformTypeFamilies eo (SigT typ kind) = flip SigT kind <$> transformTypeFamilies eo typ
transformTypeFamilies eo (InfixT typ1 n typ2) = InfixT <$> transformTypeFamilies eo typ1 <*> pure n <*> transformTypeFamilies eo typ2
transformTypeFamilies eo (UInfixT typ1 n typ2) = UInfixT <$> transformTypeFamilies eo typ1 <*> pure n <*> transformTypeFamilies eo typ2
transformTypeFamilies eo (ParensT typ) = ParensT <$> transformTypeFamilies eo typ
#if MIN_VERSION_template_haskell(2,15,0)
transformTypeFamilies eo (AppKindT typ kind) = flip AppKindT kind <$> transformTypeFamilies eo typ
transformTypeFamilies eo (ImplicitParamT s typ) = ImplicitParamT s <$> transformTypeFamilies eo typ
#endif
transformTypeFamilies _ typ = return typ

-- | Fix a family's trailing arguments to the types a use site applies it to, so that an
-- equation whose result is one of those arguments gets a concrete result.
--
-- @Chosen f x y@, with @Chosen Terse x y = x@, has no image on its own. Applied to
-- @Chosen f Int String@ it becomes @Chosen Terse Int String = Int@, which does.
substituteTrailingArgs :: [Type] -> TySynEqn -> Q TySynEqn
#if MIN_VERSION_template_haskell(2,15,0)
substituteTrailingArgs [] eqn = return eqn
substituteTrailingArgs args (TySynEqn bndrs lhs result) = case splitLhs lhs of
  Just (famName, key : rest) | length rest == length args -> do
    let substitution = M.fromList [(n, t) | (VarT n, t) <- zip rest args]
    return $ TySynEqn Nothing (AppT (ConT famName) key) (applySubstitution substitution result)
  _ -> return (TySynEqn bndrs lhs result)
  where
    splitLhs :: Type -> Maybe (Name, [Type])
    splitLhs (ConT n) = Just (n, [])
    splitLhs (AppT f x) = do
      (n, xs) <- splitLhs f
      return (n, xs <> [x])
    splitLhs (SigT t _) = splitLhs t
    splitLhs (ParensT t) = splitLhs t
    splitLhs _ = Nothing
#else
substituteTrailingArgs _ eqn = return eqn
#endif
