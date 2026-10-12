{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}


module Data.Aeson.TypeScript.Transform (
  transformTypeFamilies
  ) where

import Control.Monad.Writer
import Data.Aeson.TypeScript.Lookup
import Data.Aeson.TypeScript.Types
import Data.Aeson.TypeScript.Util (splitAppsT, typeFamilyTableName)
import Control.Monad (unless)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Typeable
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Syntax (getQ, putQ)
import qualified Language.Haskell.TH.Lib as TH

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
          handle name keyArg trailing typeFamilyName eqnsRaw = do
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
