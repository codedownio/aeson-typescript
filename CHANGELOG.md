# Change log

## 0.4.0.0

* Add new built-in instances (Word8, Int32, Int64, Map, HashSet)
* Export TSField in the Internal module
* Avoid producing redundant constraints (for fewer warnings when using -Wredundant-constraints)
* Encode maps as mapped types (allows you to have unions as keys)
* Support mapping open type families to lookup types (+ progress on handling promoted types)
* Improve propagation of T variables in declarations
* Add support for "key types", in case you have custom implementations of FromJSONKey/ToJSONKey

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
