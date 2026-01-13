# Change log

## Unreleased

* Remove question mark in Data.Map instance
* Fix @no-emit-typescript not working for nullary constructors in simple enums

## 0.6.4.0

* Fix type for maps with non-string keys (#46, fixes #28, thanks @tfausak!)

## 0.6.3.0

* GHC 9.8 support

## 0.6.2.0

* Expose generic type constructors `T4` through `T10`. (We only exposed `T`, `T1`, `T2`, and `T3` before.)

## 0.6.1.0

* Fix a bug which caused enum formatting mode to turn off when multiple declarations were provided (#41)
* Fix some mismatch issues where an enum value doesn't match the desired string.

## 0.6.0.0

* New word instances: Word, Word16, Word32, Word64
* New instances from Data.Functor: Compose, Const, Identity, Product

## 0.5.0.0

* [#35](https://github.com/codedownio/aeson-typescript/pull/35)
    * Add `Data.Aeson.TypeScript.LegalName` module for checking whether a name is a legal JavaScript name or not.
    * The `defaultFormatter` will `error` if the name contains illegal characters.
* Be able to transfer Haddock comments to emitted TypeScript (requires GHC >= 9.2 and `-haddock` flag)
* Add support for @no-emit-typescript in Haddocks for constructors and record fields (requires GHC >= 9.2 and `-haddock` flag)
* Support GHC 9.6.1

## 0.4.2.0

* Fix TypeScript (A.KeyMap a) instance

## 0.4.1.0

* Add TypeScript Int16
* Add TypeScript (A.KeyMap a) instance for aeson 2

## 0.4.0.0

* Add new built-in instances (Word8, Int32, Int64, Map, HashSet)
* Export TSField in the Internal module
* Avoid producing redundant constraints (for fewer warnings when using -Wredundant-constraints)
* Encode maps as mapped types (allows you to have unions as keys)
* Support mapping open type families to lookup types (+ progress on handling promoted types)
* Improve propagation of T variables in declarations
* Add support for "key types", in case you have custom implementations of FromJSONKey/ToJSONKey
* Add ability to recursively derive missing instances (fragile)

## 0.3.0.1

* Support GHC 9.0.1

## 0.3.0.0

* Update th-abstraction dependency to < 0.5 to support working with newer Stack LTS.
* Major refactors to improve TH quality.
* Tracking of parent types to allow recursive deriving
  * The `getParentTypes` function was added to the main typeclass.
  * The new `Data.Aeson.TypeScript.Recursive` module for working with recursive definitions.
* New support for mapping Haskell closed type families to TypeScript lookup types.

## 0.2.0.0

* New formatting option `interfaceNameModifier`.

## 0.1.0.0

* Initial release.
