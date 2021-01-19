{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Live2 where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH

       
data TestT a = TestT {
  listOfA :: [a]
  , maybeA :: Maybe a
  }
$(deriveTypeScript A.defaultOptions ''TestT)

data HigherKind a = HigherKind { higherKindList :: [a] }
$(deriveTypeScript A.defaultOptions ''HigherKind)

