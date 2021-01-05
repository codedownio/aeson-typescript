{-# LANGUAGE CPP #-}

module Data.Aeson.TypeScript.Formatting where

import Data.Aeson as A
import Data.Aeson.TypeScript.Types
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Function ((&))
import qualified Data.List as L
import Data.Maybe
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
formatTSDeclaration (FormattingOptions{..}) (TSTypeAlternatives name genericVariables names maybeDoc) =
    makeDocPrefix maybeDoc <> mainDeclaration
  where
    mainDeclaration = case chooseTypeAlternativesFormat typeAlternativesFormat of
        Enum -> [i|#{exportPrefix exportMode}enum #{typeNameModifier name} { #{alternativesEnum} }|]
          where
            alternativesEnum = T.intercalate ", " $ [toEnumName entry <> "=" <> entry | entry <- T.pack <$> names]
        EnumWithType -> [i|#{exportPrefix exportMode}enum #{typeNameModifier name}Enum { #{alternativesEnumWithType} }#{enumType}|]
          where
            alternativesEnumWithType = T.intercalate ", " $ [toEnumName entry <> "=" <> entry | entry <- T.pack <$> names]
            enumType = [i|\n\ntype #{name} = keyof typeof #{typeNameModifier name}Enum;|] :: T.Text
        TypeAlias -> [i|#{exportPrefix exportMode}type #{typeNameModifier name}#{getGenericBrackets genericVariables} = #{alternatives};|]
          where
            alternatives = T.intercalate " | " (fmap T.pack names)

    -- Only allow certain formats if some checks pass
    chooseTypeAlternativesFormat Enum
        | all isDoubleQuotedString names = Enum
        | otherwise = TypeAlias
    chooseTypeAlternativesFormat EnumWithType
        | all isDoubleQuotedString names = EnumWithType
        | otherwise = TypeAlias
    chooseTypeAlternativesFormat x = x

    isDoubleQuotedString s = case A.eitherDecode (BL8.pack s) of
        Right (A.String _) -> True
        _ -> False

    toEnumName = T.replace "\"" ""
formatTSDeclaration (FormattingOptions{..}) (TSInterfaceDeclaration interfaceName genericVariables (filter (not . isNoEmitTypeScriptField) -> members) maybeDoc) =
    makeDocPrefix maybeDoc
        <> [i|#{exportPrefix exportMode}interface #{modifiedInterfaceName}#{getGenericBrackets genericVariables} {
#{ls}
}|]
  where
    ls = T.intercalate "\n" $ [indentTo numIndentSpaces (T.pack (formatTSField member <> ";")) | member <- members]
    modifiedInterfaceName = (\(li, name) -> li <> interfaceNameModifier name) . splitAt 1 $ interfaceName

    formatTSField :: TSField -> String
    formatTSField (TSField optional name typ maybeDoc') = makeDocPrefix maybeDoc' <> [i|#{name}: #{typ}#{if optional then ("| null" :: String) else ""}|]
formatTSDeclaration _ (TSRawDeclaration text) = text

indentTo :: Int -> T.Text -> T.Text
indentTo numIndentSpaces input = T.intercalate "\n" [padding <> line | line <- T.splitOn "\n" input]
  where
    padding = T.replicate numIndentSpaces " "

exportPrefix :: ExportMode -> String
exportPrefix ExportEach = "export "
exportPrefix ExportNone = ""

-- | Format a list of TypeScript declarations into a string, suitable for putting directly into a @.d.ts@ file.
formatTSDeclarations' :: FormattingOptions -> [TSDeclaration] -> String
formatTSDeclarations' options allDeclarations =
    declarations
        & fmap (T.pack . formatTSDeclaration options)
        & T.intercalate "\n\n"
        & T.unpack
  where
    removedDeclarationNames = mapMaybe getDeclarationName (filter isNoEmitTypeScriptDeclaration allDeclarations)
      where
        getDeclarationName :: TSDeclaration -> Maybe String
        getDeclarationName (TSInterfaceDeclaration{..}) = Just interfaceName
        getDeclarationName (TSTypeAlternatives{..}) = Just typeName
        getDeclarationName _ = Nothing

    removeReferencesToRemovedNames :: [String] -> TSDeclaration -> TSDeclaration
    removeReferencesToRemovedNames removedNames decl@(TSTypeAlternatives{..}) = decl{alternativeTypes = [x | x <- alternativeTypes, not (x `L.elem` removedNames)]}
    removeReferencesToRemovedNames _ x = x

    declarations =
        allDeclarations
            & filter (not . isNoEmitTypeScriptDeclaration)
            & fmap (removeReferencesToRemovedNames removedDeclarationNames)

makeDocPrefix :: Maybe String -> String
makeDocPrefix maybeDoc = case maybeDoc of
    Nothing -> ""
    Just (T.pack -> text) ->
        ["// " <> line | line <- T.splitOn "\n" text]
            & T.intercalate "\n"
            & (<> "\n")
            & T.unpack

getGenericBrackets :: [String] -> String
getGenericBrackets [] = ""
getGenericBrackets xs = [i|<#{T.intercalate ", " (fmap T.pack xs)}>|]

-- * Support for @no-emit-typescript

noEmitTypeScriptAnnotation :: String
noEmitTypeScriptAnnotation = "@no-emit-typescript"

isNoEmitTypeScriptField :: TSField -> Bool
isNoEmitTypeScriptField (TSField{fieldDoc = (Just doc)}) = noEmitTypeScriptAnnotation `L.isInfixOf` doc
isNoEmitTypeScriptField _ = False

isNoEmitTypeScriptDeclaration :: TSDeclaration -> Bool
isNoEmitTypeScriptDeclaration (TSInterfaceDeclaration{interfaceDoc = (Just doc)}) = noEmitTypeScriptAnnotation `L.isInfixOf` doc
isNoEmitTypeScriptDeclaration (TSTypeAlternatives{typeDoc = (Just doc)}) = noEmitTypeScriptAnnotation `L.isInfixOf` doc
isNoEmitTypeScriptDeclaration _ = False
