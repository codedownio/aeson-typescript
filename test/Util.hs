{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

module Util where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import qualified Data.ByteString.Lazy as B
import Data.Proxy
import Data.String
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Shelly hiding ((</>))
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

tsc = "test_assets/node_modules/.bin/tsc"

testTypeCheck :: forall a. (TypeScript a, ToJSON a) => a -> IO ()
testTypeCheck obj = withSystemTempDirectory "typescript_test" $ \folder -> do
  let tsFile = folder </> "test.ts"

  writeFile tsFile [i|
#{formatTSDeclarations tsDeclarations}

let x: #{tsType} = #{A.encode obj};
|]

  ensureTSCExists

  -- "--diagnostics", "--listFiles"
  shelly $ bash (fromString tsc) ["--noEmit", "--skipLibCheck", "--traceResolution", "--noResolve", T.pack tsFile]

  return ()
  where tsDeclarations :: [TSDeclaration] = getTypeScriptDeclaration (Proxy :: Proxy a)
        tsType :: String = getTypeScriptType (Proxy :: Proxy a)

data ProxyAndValue a = ProxyAndValue (Proxy a) a

class IsProxyAndValue a b where
  getProxy :: a -> Proxy b
  getValue :: a -> b

instance IsProxyAndValue (ProxyAndValue a) a where
  getProxy (ProxyAndValue proxy _) = proxy
  getValue (ProxyAndValue _ value) = value

data PAV = forall a b. IsProxyAndValue a b => PAV a b

instance (IsProxyAndValue a b) => IsProxyAndValue (PAV) b where
  getProxy (PAV x) = getProxy x
  getValue (PAV x) = getValue x

testTypeCheckDeclarations :: (TypeScript a, ToJSON a) => [TSDeclaration] -> [PAV] -> IO ()
testTypeCheckDeclarations tsDeclarations proxiesAndVals = withSystemTempDirectory "typescript_test" $ \folder -> do
  let tsFile = folder </> "test.ts"

  writeFile tsFile [i|
#{formatTSDeclarations tsDeclarations}

#{T.unlines typeLines}
|]

  ensureTSCExists

  shelly $ bash (fromString tsc) ["--noEmit", "--skipLibCheck", "--traceResolution", "--noResolve", T.pack tsFile]

  return ()
  where typesAndVals = [(getTypeScriptType (getProxy x), A.encode (getValue x)) | x <- proxiesAndVals]
        typeLines = [[i|let x#{index}: #{typ} = #{val};|] | (index, (typ, val)) <- zip [1..] typesAndVals]


ensureTSCExists :: IO ()
ensureTSCExists = doesFileExist tsc >>= \exists -> when (not exists) $ void $ do
  putStrLn "Invoking yarn to install tsc compiler (make sure yarn is installed)"
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode ((shell "yarn install") {cwd = Just "test_assets"}) ""
  when (exitCode /= ExitSuccess) $ putStrLn [i|Error installing yarn: '#{stderr}', '#{stdout}'|]
