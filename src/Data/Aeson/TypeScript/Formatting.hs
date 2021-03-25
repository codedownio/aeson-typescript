{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, CPP #-}

module Data.Aeson.TypeScript.Formatting where

import Data.Aeson.TypeScript.Types
import Data.String.Interpolate
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
  case typeAlternativesFormat of
    Enum -> [i|#{exportPrefix exportMode}enum #{typeNameModifier name} { #{alternativesEnum} }|]
    EnumWithType -> [i|#{exportPrefix exportMode}enum #{typeNameModifier name} { #{alternativesEnumWithType} }#{enumType}|]
    TypeAlias -> [i|#{exportPrefix exportMode}type #{typeNameModifier name}#{getGenericBrackets genericVariables} = #{alternatives};|]
  where
    alternatives = T.intercalate " | " (fmap T.pack names)
    alternativesEnum = T.intercalate ", " $ [toEnumName entry | entry <- T.pack <$> names]
    alternativesEnumWithType = T.intercalate ", " $ [toEnumName entry <> "=" <> entry | entry <- T.pack <$> names]
    enumType = [i|\n\ntype #{name} = keyof typeof #{typeNameModifier name};|] :: T.Text
    toEnumName = T.replace "\"" ""

formatTSDeclaration (FormattingOptions {..}) (TSInterfaceDeclaration interfaceName genericVariables members) =
  [i|#{exportPrefix exportMode}interface #{modifiedInterfaceName}#{getGenericBrackets genericVariables} {
#{ls}
}|] where ls = T.intercalate "\n" $ fmap T.pack [(replicate numIndentSpaces ' ') <> formatTSField member <> ";"| member <- members]
          modifiedInterfaceName = (\(li, name) -> li <> interfaceNameModifier name) . splitAt 1 $ interfaceName

formatTSDeclaration (FormattingOptions {..}) (TSRawDeclaration text) = text

exportPrefix :: ExportMode -> String
exportPrefix ExportEach = "export "
exportPrefix ExportNone = ""

-- | Format a list of TypeScript declarations into a string, suitable for putting directly into a @.d.ts@ file.
formatTSDeclarations' :: FormattingOptions -> [TSDeclaration] -> String
formatTSDeclarations' options declarations = T.unpack $ T.intercalate "\n\n" (fmap (T.pack . formatTSDeclaration (validateFormattingOptions options declarations)) declarations)

validateFormattingOptions :: FormattingOptions -> [TSDeclaration] -> FormattingOptions
validateFormattingOptions options@FormattingOptions{..} decls
  | typeAlternativesFormat == Enum && isPlainSumType decls = options
  | typeAlternativesFormat == EnumWithType && isPlainSumType decls = options { typeNameModifier = flip (<>) "Enum" }
  | otherwise = options { typeAlternativesFormat = TypeAlias }
  where
    isInterface :: TSDeclaration -> Bool
    isInterface TSInterfaceDeclaration{} = True
    isInterface _ = False

    -- Plain sum types have only one declaration with multiple alternatives
    -- Units (data U = U) contain two declarations, and thus are invalid
    isPlainSumType ds = (not . any isInterface $ ds) && length ds == 1

formatTSField :: TSField -> String
formatTSField (TSField optional name typ) = [i|#{name}#{if optional then ("?" :: String) else ""}: #{typ}|]

getGenericBrackets :: [String] -> String
getGenericBrackets [] = ""
getGenericBrackets xs = [i|<#{T.intercalate ", " (fmap T.pack xs)}>|]
