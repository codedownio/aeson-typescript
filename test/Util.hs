{-# LANGUAGE CPP #-}

module Util where

import Control.Monad
import Data.Aeson as A
import Data.Aeson.TypeScript.TH
import qualified Data.ByteString.Lazy as B
import Data.Proxy
import Data.String.Interpolate
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process hiding (cwd)


npmInstallScript, yarnInstallScript, localTSC :: String
npmInstallScript = "test/assets/npm_install.sh"
yarnInstallScript = "test/assets/yarn_install.sh"
localTSC = "test/assets/node_modules/.bin/tsc"

isCI :: IO Bool
isCI = lookupEnv "CI" >>= (return . (== (Just "true")))

getTSC :: IO FilePath
getTSC = isCI >>= \case
  True -> do
    return "tsc" -- Assume it's set up on the path
  False -> do
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
  void $ readProcess tsc ["--noEmit", "--skipLibCheck", "--traceResolution", "--noResolve", tsFile] ""

  return ()
  where tsDeclarations :: [TSDeclaration] = getTypeScriptDeclarations (Proxy :: Proxy a)
        tsType :: String = getTypeScriptType (Proxy :: Proxy a)


getTSFile :: [TSDeclaration] -> [(String, B.ByteString)] -> String
getTSFile tsDeclarations typesAndVals = [i|
#{formatTSDeclarations tsDeclarations}

#{T.unlines typeLines}
|]
  where typeLines = [[i|let x#{index}: #{typ} = #{val};|] | (index, (typ, val)) <- zip [(1 :: Int)..] typesAndVals]


testTypeCheckDeclarations :: [TSDeclaration] -> [(String, B.ByteString)] -> IO ()
testTypeCheckDeclarations tsDeclarations typesAndVals = withSystemTempDirectory "typescript_test" $ \folder -> do
  let tsFile = folder </> "test.ts"

  let contents = getTSFile tsDeclarations typesAndVals

  writeFile tsFile contents

  tsc <- getTSC
  (code, sout, serr) <- readProcessWithExitCode tsc ["--strict", "--noEmit", "--skipLibCheck", "--traceResolution", "--noResolve", tsFile] ""

  when (code /= ExitSuccess) $
    error [__i|TSC check failed.
               File contents:
               #{contents}

               Stdout:
               #{sout}

               Stderr:
               #{serr}
              |]


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
