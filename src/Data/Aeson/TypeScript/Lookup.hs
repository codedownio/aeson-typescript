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
import Data.Proxy
import Data.String.Interpolate
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
    FamilyI (ClosedTypeFamilyD (TypeFamilyHead _name _vars _sig _maybeInject) eqns) _decs -> do
      interfaceDecl <- getClosedTypeFamilyInterfaceDecl name eqns
      return [FunD (mkName declNameStr) [Clause [] (NormalB (ListE [interfaceDecl])) []]]

    _ -> fail [i|Expected a close type family; got #{info}|]

getClosedTypeFamilyInterfaceDecl :: Name -> [TySynEqn] -> Q Exp
getClosedTypeFamilyInterfaceDecl name eqns = do
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
    x -> fail [i|aeson-typescript doesn't know yet how to handle this type family equation: '#{x}'|]

  [| TSInterfaceDeclaration $(TH.stringE $ nameBase name) [] (L.sortBy (compare `on` fieldName) $(listE $ fmap return fields)) Nothing |]

getClosedTypeFamilyImage :: [TySynEqn] -> Q [Type]
getClosedTypeFamilyImage eqns = do
  forM eqns $ \case
#if MIN_VERSION_template_haskell(2,15,0)
    TySynEqn Nothing (AppT (ConT _) _) result -> return result
#else
    TySynEqn [ConT _] result -> return result
#endif
    x -> fail [i|aeson-typescript doesn't know yet how to handle this type family equation: '#{x}'|]
