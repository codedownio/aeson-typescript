{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module Util where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import Data.Aeson.TypeScript.Types
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


getTSFile tsDeclarations typesAndVals = [i|
#{formatTSDeclarations tsDeclarations}

#{T.unlines typeLines}
|]
  where typeLines = [[i|let x#{index}: #{typ} = #{val};|] | (index, (typ, val)) <- zip [1..] typesAndVals]


testTypeCheckDeclarations :: [TSDeclaration] -> [(String, B.ByteString)] -> IO ()
testTypeCheckDeclarations tsDeclarations typesAndVals = withSystemTempDirectory "typescript_test" $ \folder -> do
  let tsFile = folder </> "test.ts"

  let contents = getTSFile tsDeclarations typesAndVals

  writeFile tsFile contents

  ensureTSCExists

  (output, code) <- shelly $ errExit False $ do
      output <- bash (fromString tsc) ["--noEmit", "--skipLibCheck", "--traceResolution", "--noResolve", T.pack tsFile]
      code <- lastExitCode
      return (output, code)

  when (code /= 0) $ do
    putStrLn [i|TSC check failed. File contents were\n\n#{contents}|]
    error [i|TSC check failed: #{output}|]

  return ()


ensureTSCExists :: IO ()
ensureTSCExists = doesFileExist tsc >>= \exists -> when (not exists) $ void $ do
  putStrLn "Invoking yarn to install tsc compiler (make sure yarn is installed)"
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode ((shell "yarn install") {cwd = Just "test_assets"}) ""
  when (exitCode /= ExitSuccess) $ putStrLn [i|Error installing yarn: '#{stderr}', '#{stdout}'|]
