{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Live3 where

import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Function
import Data.Proxy


data Test = TestBlah {x :: Int, y :: Bool}

$(deriveTypeScript (A.defaultOptions { A.tagSingleConstructors = True }) ''Test)

main :: IO ()
main = getTypeScriptDeclarations (Proxy @Test)
     & formatTSDeclarations
     & putStrLn
