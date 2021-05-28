# Change log

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
