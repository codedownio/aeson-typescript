{-# LANGUAGE UndecidableInstances #-}

module TestBoilerplate where

import Control.Monad.Writer.Lazy hiding (Product)
import qualified Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.TH
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Product
import Data.Kind
import Data.List.NonEmpty
import Data.Proxy
import Data.String.Interpolate
import Data.Word
import Language.Haskell.TH hiding (Type)
import Numeric.Natural (Natural)
import Test.Hspec
import Util
import Util.Aeson

data Unit = Unit
data OneFieldRecordless = OneFieldRecordless Int
data OneField = OneField { simpleString :: String }
data TwoFieldRecordless = TwoFieldRecordless Int String
data TwoField = TwoField { doubleInt :: Int, doubleString :: String }
data Hybrid = HybridSimple Int | HybridRecord { hybridString :: String }
data TwoConstructor = Con1 { con1String :: String } | Con2 { con2String :: String, con2Int :: Int }
data Complex a = Nullary | Unary Int | Product String Char a | Record { testOne :: Int, testTwo :: Bool, testThree :: Complex a} deriving Eq
data OptionalRecord = OptionalRecord {optionalInt :: Maybe Int}
data OptionalTuple1 = OptionalTuple1 (Maybe Int)
data OptionalTuple2 = OptionalTuple2 String (Maybe Int)
data AesonTypes = AesonTypes { aesonValue :: A.Value, aesonObject :: A.Object }
data Numbers = Numbers {
  natural :: Natural
  , word :: Word
  , word16 :: Word16
  , word32 :: Word32
  , word64 :: Word64
  }
data FancyFunctors = FancyFunctors {
  nonEmpty :: NonEmpty Int
  , const :: Const Int Int
  , product :: Product Identity Identity Int
  , compose :: Compose Identity Identity Int
  }

-- * Values

fancyFunctorsValue :: FancyFunctors
fancyFunctorsValue = FancyFunctors (42 :| []) (Const 42) (Pair 42 42) (Compose 42)

-- * For testing type families

instance TypeScript Identity where getTypeScriptType _ = "any"

data SingleDE = SingleDE
instance TypeScript SingleDE where getTypeScriptType _ = [i|"single"|]

data K8SDE = K8SDE
instance TypeScript K8SDE where getTypeScriptType _ = [i|"k8s"|]

data SingleNodeEnvironment = SingleNodeEnvironment deriving (Eq, Show)
instance TypeScript SingleNodeEnvironment where getTypeScriptType _ = [i|"single_node_env"|]

data K8SEnvironment = K8SEnvironment deriving (Eq, Show)
instance TypeScript K8SEnvironment where getTypeScriptType _ = [i|"k8s_env"|]

data Nullable (c :: Type -> Type) x
data Exposed x
type family Columnar (f :: Type -> Type) x where
    Columnar Exposed x = Exposed x
    Columnar Identity x = x
    Columnar (Nullable c) x = Columnar c (Maybe x)
    Columnar f x = f x

-- * Declarations

testDeclarations :: String -> A.Options -> Q [Dec]
testDeclarations testName aesonOptions = do
  decls :: [Dec] <- execWriterT $ do
    deriveInstances ''Unit
    deriveInstances ''OneFieldRecordless
    deriveInstances ''OneField
    deriveInstances ''TwoFieldRecordless
    deriveInstances ''TwoField
    deriveInstances ''Hybrid
    deriveInstances ''TwoConstructor
    deriveInstances ''Complex
    deriveInstances ''OptionalRecord
    deriveInstances ''OptionalTuple1
    deriveInstances ''OptionalTuple2
    deriveInstances ''AesonTypes
    deriveInstances ''Numbers
    deriveInstances ''FancyFunctors

  typesAndValues :: Exp <- [e|[(getTypeScriptType (Proxy :: Proxy Unit), A.encode Unit)

                              , (getTypeScriptType (Proxy :: Proxy OneFieldRecordless), A.encode $ OneFieldRecordless 42)

                              , (getTypeScriptType (Proxy :: Proxy OneField), A.encode $ OneField "asdf")

                              , (getTypeScriptType (Proxy :: Proxy TwoFieldRecordless), A.encode $ TwoFieldRecordless 42 "asdf")

                              , (getTypeScriptType (Proxy :: Proxy TwoField), A.encode $ TwoField 42 "asdf")

                              , (getTypeScriptType (Proxy :: Proxy TwoConstructor), A.encode $ Con1 "asdf")
                              , (getTypeScriptType (Proxy :: Proxy TwoConstructor), A.encode $ Con2 "asdf" 42)

                              , (getTypeScriptType (Proxy :: Proxy Hybrid), A.encode $ HybridSimple 42)
                              , (getTypeScriptType (Proxy :: Proxy Hybrid), A.encode $ HybridRecord "asdf")

                              , (getTypeScriptType (Proxy :: Proxy (Complex Int)), A.encode (Nullary :: Complex Int))
                              , (getTypeScriptType (Proxy :: Proxy (Complex Int)), A.encode (Unary 42 :: Complex Int))
                              , (getTypeScriptType (Proxy :: Proxy (Complex Int)), A.encode (Product "asdf" 'g' 42 :: Complex Int))
                              , (getTypeScriptType (Proxy :: Proxy (Complex Int)), A.encode ((Record { testOne = 3, testTwo = True, testThree = Product "test" 'A' 123}) :: Complex Int))

                              , (getTypeScriptType (Proxy :: Proxy OptionalRecord), A.encode (OptionalRecord { optionalInt = Nothing }))
                              , (getTypeScriptType (Proxy :: Proxy OptionalRecord), A.encode (OptionalRecord { optionalInt = Just 1 }))

                              , (getTypeScriptType (Proxy :: Proxy OptionalTuple1), A.encode (OptionalTuple1 Nothing))
                              , (getTypeScriptType (Proxy :: Proxy OptionalTuple1), A.encode (OptionalTuple1 (Just 1)))

                              , (getTypeScriptType (Proxy :: Proxy OptionalTuple2), A.encode (OptionalTuple2 "asdf" Nothing))
                              , (getTypeScriptType (Proxy :: Proxy OptionalTuple2), A.encode (OptionalTuple2 "asdf" (Just 1)))

                              , (getTypeScriptType (Proxy :: Proxy AesonTypes), A.encode (AesonTypes {
                                                                                             aesonValue = A.object [("foo" :: AesonKey, A.Number 42)]
                                                                                             , aesonObject = aesonFromList [("foo", A.Number 42)]
                                                                                             }))

                              , (getTypeScriptType (Proxy :: Proxy Numbers), A.encode (Numbers 42 42 42 42 42))
                              , (getTypeScriptType (Proxy :: Proxy FancyFunctors), A.encode fancyFunctorsValue)
                              ]|]

  declarations :: Exp <- [e|getTypeScriptDeclarations (Proxy :: Proxy Unit)
                         <> getTypeScriptDeclarations (Proxy :: Proxy OneFieldRecordless)
                         <> getTypeScriptDeclarations (Proxy :: Proxy OneField)
                         <> getTypeScriptDeclarations (Proxy :: Proxy TwoFieldRecordless)
                         <> getTypeScriptDeclarations (Proxy :: Proxy TwoField)
                         <> getTypeScriptDeclarations (Proxy :: Proxy Hybrid)
                         <> getTypeScriptDeclarations (Proxy :: Proxy TwoConstructor)
                         <> getTypeScriptDeclarations (Proxy :: Proxy (Complex T))
                         <> getTypeScriptDeclarations (Proxy :: Proxy OptionalRecord)
                         <> getTypeScriptDeclarations (Proxy :: Proxy OptionalTuple1)
                         <> getTypeScriptDeclarations (Proxy :: Proxy OptionalTuple2)
                         <> getTypeScriptDeclarations (Proxy :: Proxy AesonTypes)
                         <> getTypeScriptDeclarations (Proxy :: Proxy Numbers)
                         <> getTypeScriptDeclarations (Proxy :: Proxy FancyFunctors)
                         |]

  tests <- [d|tests :: SpecWith ()
              tests = describe $(return $ LitE $ StringL testName) $ it "type checks everything with tsc" $ testTypeCheckDeclarations $(return declarations) $(return typesAndValues)|]

  return $ decls ++ tests

  where deriveInstances :: Name -> WriterT [Dec] Q ()
        deriveInstances name = do
          writeM $ deriveJSON aesonOptions name
          writeM $ deriveTypeScript aesonOptions name

writeM :: (Monad m) => m w -> WriterT w m ()
writeM action = WriterT $ action >>= \w -> return ((), w)
