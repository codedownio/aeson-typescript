{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Data.Aeson.TypeScript.Lookup where

import Control.Monad
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Types
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Data.Proxy
import Data.String.Interpolate
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype.TyVarBndr ()
import Language.Haskell.TH.Datatype (applySubstitution)
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

    _ -> fail [i|Expected a closed type family; got #{info}|]

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

getClosedTypeFamilyInterfaceDecl :: Name -> [TySynEqn] -> Q Exp
getClosedTypeFamilyInterfaceDecl name eqns = getClosedTypeFamilyInterfaceDecl' (nameBase name) eqns

getClosedTypeFamilyInterfaceDecl' :: String -> [TySynEqn] -> Q Exp
getClosedTypeFamilyInterfaceDecl' name eqns = do
  fields <- forM eqns $ \case
#if MIN_VERSION_template_haskell(2,15,0)
    TySynEqn Nothing (AppT (ConT _) (ConT arg)) result -> do
      [| TSField False (getTypeScriptType (Proxy :: Proxy $(conT arg))) (getTypeScriptType (Proxy :: Proxy $(return result))) Nothing |]
    TySynEqn Nothing (AppT (ConT _) (PromotedT arg)) result -> do
      [| TSField False (getTypeScriptType (Proxy :: Proxy $(promotedT arg))) (getTypeScriptType (Proxy :: Proxy $(return result))) Nothing |]
#else
    TySynEqn [ConT arg] result -> do
      [| TSField False (getTypeScriptType (Proxy :: Proxy $(conT arg))) (getTypeScriptType (Proxy :: Proxy $(return result))) Nothing |]
#endif
    x -> fail [i|aeson-typescript doesn't know yet how to handle this type family equation when generating interface declaration: '#{x}'|]

  [| TSInterfaceDeclaration $(TH.stringE name) [] (L.sortBy (compare `on` fieldName) $(listE $ fmap return fields)) Nothing |]

getClosedTypeFamilyImage :: [TySynEqn] -> Q [Type]
getClosedTypeFamilyImage eqns = do
  forM eqns $ \case
#if MIN_VERSION_template_haskell(2,15,0)
    TySynEqn Nothing (AppT (ConT _) _) result -> return result
#else
    TySynEqn [ConT _] result -> return result
#endif
    x -> fail [i|aeson-typescript doesn't know yet how to handle this type family equation when calculating closed type family image: '#{x}'|]
