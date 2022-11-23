-- | This module defines functions which are useful for determing if
-- a given name is a legal JavaScript name accord to <https://stackoverflow.com/questions/1661197/what-characters-are-valid-for-javascript-variable-names this post>.
module Data.Aeson.TypeScript.LegalName where

import qualified Data.Set as Set
import Language.Haskell.TH
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Char
import Data.Foldable


-- | This reports a compile-time error if the given 'Name' contains
-- characters that are not allowed in a JavaScript name.
checkIllegalName :: Name -> Q ()
checkIllegalName =
  checkIllegalNameString . nameBase

-- | As 'checkIllegalName', but operates on the underlying 'String'.
checkIllegalNameString :: String -> Q ()
checkIllegalNameString nameStr =
  case NonEmpty.nonEmpty nameStr of
    Just nameChars ->
      traverse_ (traverse_ (reportError . message)) . checkIllegalNameChars $ nameChars
    Nothing ->
      reportError "checkIllegalName called with an empty name somehow??"
  where
    message c =
      concat ["The name ", nameStr, " has an illegal character: ", show c]

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
