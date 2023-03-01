{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, RecordWildCards, ScopedTypeVariables, NamedFieldPuns, CPP #-}

module Data.Aeson.TypeScript.Formatting where

import Data.Aeson.TypeScript.Types
import qualified Data.List as L
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
formatTSDeclaration (FormattingOptions {..}) (TSTypeAlternatives name genericVariables names maybeDoc) =
  makeDocPrefix maybeDoc <> mainDeclaration
  where
    mainDeclaration = case typeAlternativesFormat of
      Enum -> [i|#{exportPrefix exportMode}enum #{typeNameModifier name} { #{alternativesEnum} }|]
      EnumWithType -> [i|#{exportPrefix exportMode}enum #{typeNameModifier name} { #{alternativesEnumWithType} }#{enumType}|]
      TypeAlias -> [i|#{exportPrefix exportMode}type #{typeNameModifier name}#{getGenericBrackets genericVariables} = #{alternatives};|]

    alternatives = T.intercalate " | " (fmap T.pack names)
    alternativesEnum = T.intercalate ", " $ [toEnumName entry | entry <- T.pack <$> names]
    alternativesEnumWithType = T.intercalate ", " $ [toEnumName entry <> "=" <> entry | entry <- T.pack <$> names]
    enumType = [i|\n\ntype #{name} = keyof typeof #{typeNameModifier name};|] :: T.Text
    toEnumName = T.replace "\"" ""

formatTSDeclaration (FormattingOptions {..}) (TSInterfaceDeclaration interfaceName genericVariables members maybeDoc) =
  makeDocPrefix maybeDoc <> [i|#{exportPrefix exportMode}interface #{modifiedInterfaceName}#{getGenericBrackets genericVariables} {
#{ls}
}|] where
      ls = T.intercalate "\n" $ [indentTo numIndentSpaces (T.pack (formatTSField member <> ";")) | member <- members]
      modifiedInterfaceName = (\(li, name) -> li <> interfaceNameModifier name) . splitAt 1 $ interfaceName

formatTSDeclaration _ (TSRawDeclaration text) = text

indentTo :: Int -> T.Text -> T.Text
indentTo numIndentSpaces input = T.intercalate "\n" [padding <> line | line <- T.splitOn "\n" input]
  where padding = T.replicate numIndentSpaces " "

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
formatTSField (TSField optional name typ maybeDoc) = makeDocPrefix maybeDoc <> [i|#{name}#{if optional then ("?" :: String) else ""}: #{typ}|]

makeDocPrefix :: Maybe String -> String
makeDocPrefix maybeDoc = case maybeDoc of
  Nothing -> ""
  Just doc | '\n' `L.elem` doc -> "/* " <> doc <> " */\n"
  Just doc -> "// " <> doc <> "\n"

getGenericBrackets :: [String] -> String
getGenericBrackets [] = ""
getGenericBrackets xs = [i|<#{T.intercalate ", " (fmap T.pack xs)}>|]
