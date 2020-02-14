{-# LANGUAGE CPP, QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, LambdaCase #-}

module Util where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import qualified Data.ByteString.Lazy as B
import Data.Proxy
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

npmInstallScript = "test/assets/npm_install.sh"
yarnInstallScript = "test/assets/yarn_install.sh"
localTSC = "test/assets/node_modules/.bin/tsc"

isCI :: IO Bool
isCI = lookupEnv "CI" >>= (return . (== (Just "true")))

getTSC :: IO FilePath
getTSC = do
  isCI <- isCI
  case isCI of
    True -> do
      return "tsc" -- Assume it's set up on the path
    False ->
      findExecutable "tsc" >>= \case
        Just tsc -> pure tsc
        Nothing -> do
         ensureTSCExists
         return localTSC

testTypeCheck :: forall a. (TypeScript a, ToJSON a) => a -> IO ()
testTypeCheck obj = withSystemTempDirectory "typescript_test" $ \folder -> do
  let tsFile = folder </> "test.ts"

  writeFile tsFile [i|
#{formatTSDeclarations tsDeclarations}

let x: #{tsType} = #{A.encode obj};
|]

  -- "--diagnostics", "--listFiles"
  tsc <- getTSC
  readProcess tsc ["--noEmit", "--skipLibCheck", "--traceResolution", "--noResolve", tsFile] ""

  return ()
  where tsDeclarations :: [TSDeclaration] = getTypeScriptDeclarations (Proxy :: Proxy a)
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

  tsc <- getTSC
  (code, output, err) <- readProcessWithExitCode tsc ["--noEmit", "--skipLibCheck", "--traceResolution", "--noResolve", tsFile] ""

  when (code /= ExitSuccess) $ do
    error [i|TSC check failed: #{output}. File contents were\n\n#{contents}|]

  return ()


ensureTSCExists :: IO ()
ensureTSCExists = doesFileExist localTSC >>= \exists -> unless exists $ void $ do
  cwd <- getCurrentDirectory

  installScript <- chooseInstallScript

  putStrLn [i|Invoking yarn to install tsc compiler (make sure yarn is installed). CWD is #{cwd}|]
  (exitCode, stdout, stderr) <- readProcessWithExitCode installScript [] ""
  when (exitCode /= ExitSuccess) $ putStrLn [i|Error installing yarn: '#{stderr}', '#{stdout}'|]


-- Between Aeson 1.1.2.0 and 1.2.0.0, tagSingleConstructors was added
setTagSingleConstructors :: Options -> Options
#if MIN_VERSION_aeson(1,2,0)
setTagSingleConstructors options = options {tagSingleConstructors=True}
#else
setTagSingleConstructors = id
#endif

chooseInstallScript :: IO String
chooseInstallScript = do
  yarn <- findExecutable "yarn"
  case yarn of
    Just _ -> return yarnInstallScript
    Nothing -> do
      npm <- findExecutable "npm"
      case npm of
        Just _ -> return npmInstallScript
        Nothing -> error [i|Couldn't find either yarn or npm; one of them is needed to install a TypeScript compiler.|]
