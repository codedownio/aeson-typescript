{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, OverloadedStrings #-}

module Lib where

import Control.Monad.Writer.Lazy
import Data.Data
import Data.Monoid
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable

data Foo = Foo { fooString :: String
               , fooInt :: Int }
         | Bar { barString :: String
               , barMaybe :: Maybe Int
               } deriving (Data, Typeable)

getTypescript :: DataType -> Writer T.Text ()
getTypescript dataType | isAlgType dataType = do
                           interfaces <- sequence [getInterface x | x <- dataTypeConstrs dataType]
                           tell [i|type #{dataTypeName dataType} = #{T.intercalate " | " interfaces};\n\n|] where


getInterface :: Constr -> Writer T.Text T.Text
getInterface constr = do
  tell [i|interface #{name} {
#{T.unlines fields}
}\n\n|]

  return name
    where name = [i|I#{show constr}|]
          fields = [[i|  #{x}: #{constrRep constr};|] | x <- constrFields constr]
