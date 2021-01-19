{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, CPP #-}

module Data.Aeson.TypeScript.Formatting where

import Data.Aeson.TypeScript.Types
import Data.String.Interpolate.IsString
import qualified Data.Text as T

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

       
-- | Same as 'formatTSDeclarations'', but uses default formatting options.
formatTSDeclarations :: [TSDeclaration] -> String
formatTSDeclarations = formatTSDeclarations' defaultFormattingOptions

-- | Format a single TypeScript declaration. This version accepts a FormattingOptions object in case you want more control over the output.
formatTSDeclaration :: FormattingOptions -> TSDeclaration -> String
formatTSDeclaration (FormattingOptions {..}) (TSTypeAlternatives name genericVariables names) =
  [i|type #{typeNameModifier name}#{getGenericBrackets genericVariables} = #{alternatives};|]
  where alternatives = T.intercalate " | " (fmap T.pack names)

formatTSDeclaration (FormattingOptions {..}) (TSInterfaceDeclaration interfaceName genericVariables members) =
  [i|interface #{modifiedInterfaceName}#{getGenericBrackets genericVariables} {
#{ls}
}|] where ls = T.intercalate "\n" $ fmap T.pack [(replicate numIndentSpaces ' ') <> formatTSField member <> ";"| member <- members]
          modifiedInterfaceName = (\(li, name) -> li <> interfaceNameModifier name) . splitAt 1 $ interfaceName

formatTSDeclaration (FormattingOptions {..}) (TSRawDeclaration text) = text

-- | Format a list of TypeScript declarations into a string, suitable for putting directly into a @.d.ts@ file.
formatTSDeclarations' :: FormattingOptions -> [TSDeclaration] -> String
formatTSDeclarations' options declarations = T.unpack $ T.intercalate "\n\n" (fmap (T.pack . formatTSDeclaration options) declarations)

formatTSField :: TSField -> String
formatTSField (TSField optional name typ) = [i|#{name}#{if optional then ("?" :: String) else ""}: #{typ}|]

getGenericBrackets :: [String] -> String
getGenericBrackets [] = ""
getGenericBrackets xs = [i|<#{T.intercalate ", " (fmap T.pack xs)}>|]
