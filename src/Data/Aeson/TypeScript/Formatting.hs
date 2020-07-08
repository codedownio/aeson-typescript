{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns #-}

module Data.Aeson.TypeScript.Formatting where

import Data.Aeson.TypeScript.Types
import Data.Monoid
import Data.String.Interpolate.IsString
import qualified Data.Text as T

-- | Same as 'formatTSDeclarations'', but uses default formatting options.
formatTSDeclarations :: [TSDeclaration] -> String
formatTSDeclarations = formatTSDeclarations' defaultFormattingOptions

-- | Format a single TypeScript declaration. This version accepts a FormattingOptions object in case you want more control over the output.
formatTSDeclaration :: FormattingOptions -> TSDeclaration -> String
formatTSDeclaration (FormattingOptions {..}) (TSTypeAlternatives name genericVariables names) =
  [i|#{if exportTypes then "export " else ""}type #{typeNameModifier name}#{getGenericBrackets genericVariables} = #{alternatives};|]
  where alternatives = T.intercalate " | " (fmap T.pack names)

formatTSDeclaration (FormattingOptions {..}) (TSInterfaceDeclaration interfaceName genericVariables members) =
  [i|#{if exportTypes then "export " else ""}interface #{modifiedInterfaceName}#{getGenericBrackets genericVariables} {
#{lines}
}|] where lines = T.intercalate "\n" $ fmap T.pack [(replicate numIndentSpaces ' ') <> formatTSField member <> ";"| member <- members]
          modifiedInterfaceName = (\(i, name) -> i <> interfaceNameModifier name) . splitAt 1 $ interfaceName

-- | Format a list of TypeScript declarations into a string, suitable for putting directly into a @.d.ts@ file.
formatTSDeclarations' :: FormattingOptions -> [TSDeclaration] -> String
formatTSDeclarations' options declarations = T.unpack $ T.intercalate "\n\n" (fmap (T.pack . formatTSDeclaration options) declarations)

formatTSField :: TSField -> String
formatTSField (TSField optional name typ) = [i|#{name}#{if optional then "?" else ""}: #{typ}|]

getGenericBrackets :: [String] -> String
getGenericBrackets [] = ""
getGenericBrackets xs = [i|<#{T.intercalate ", " (fmap T.pack xs)}>|]
