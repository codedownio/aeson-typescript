-- | This module defines functions which are useful for determining if
-- a given name is a legal JavaScript name according to
-- <https://stackoverflow.com/questions/1661197/what-characters-are-valid-for-javascript-variable-names this post>.
module Data.Aeson.TypeScript.LegalName where

import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set


-- | The return type is the illegal characters that are in the name. If the
-- input has no illegal characters, then you have 'Nothing'.
checkIllegalNameChars :: NonEmpty Char -> Maybe (NonEmpty Char)
checkIllegalNameChars (firstChar :| restChars) = NonEmpty.nonEmpty $
  let
    legalFirstCategories =
      Set.fromList
        [ UppercaseLetter
        , LowercaseLetter
        , TitlecaseLetter
        , ModifierLetter
        , OtherLetter
        , LetterNumber
        ]
    legalRestCategories =
      Set.fromList
        [ NonSpacingMark
        , SpacingCombiningMark
        , DecimalNumber
        , ConnectorPunctuation
        ]
        `Set.union` legalFirstCategories
    isIllegalFirstChar c = not $
      c `elem` ['$', '_'] || generalCategory c `Set.member` legalFirstCategories
    isIllegalRestChar c = not $
      generalCategory c `Set.member` legalRestCategories
  in
    filter isIllegalFirstChar [firstChar] <> filter isIllegalRestChar restChars
