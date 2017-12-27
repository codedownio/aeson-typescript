{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables #-}

module Test where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.Writer.Lazy
import qualified Data.Aeson as A
import Data.Data
import Data.Monoid
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import TH

$(deriveTypeScript A.defaultOptions ''Foo)


-- instance TypeScript Foo
--     where getTypeScriptType = "Foo"
--           getTypeScriptDeclaration = [TSTypeAlternatives ["IFoo",
--                                                                 "IBar"],
--                                          TSInterfaceDeclaration "IFoo" [TSField False "fooString" (show (getTypeScriptType :: TypeScriptString String)),
--                                                                            TSField False "fooInt" (show (getTypeScriptType :: TypeScriptString Int))],
--                                          TSInterfaceDeclaration "IBar" [TSField False "barString" (show (getTypeScriptType :: TypeScriptString String)),
--                                                                          TSField False "barMaybe" (show (getTypeScriptType :: TypeScriptString (Maybe Int)))]]
