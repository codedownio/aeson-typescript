
# Welcome to `aeson-typescript` [![Hackage](https://img.shields.io/hackage/v/aeson-typescript.svg)](https://hackage.haskell.org/package/aeson-typescript) [![Build Status](https://travis-ci.org/codedownio/aeson-typescript.svg)](https://travis-ci.org/codedownio/aeson-typescript)

This library provides a way to generate TypeScript `.d.ts` files that match your existing Aeson `ToJSON` instances.
If you already use Aeson's Template Haskell support to derive your instances, then deriving TypeScript is as simple as

```haskell
$(deriveTypeScript myAesonOptions ''MyType)
```

For example,

```haskell
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq
```

Next we derive the necessary instances.

```haskell
$(deriveTypeScript (defaultOptions {fieldLabelModifier = drop 4, constructorTagModifier = map toLower}) ''D)
```

Now we can use the newly created instances.

```haskell
>>> putStrLn $ formatTSDeclarations $ getTypeScriptDeclaration (Proxy :: Proxy D)

type D<T> = "nullary" | IUnary<T> | IProduct<T> | IRecord<T>;

type IUnary<T> = number;

type IProduct<T> = [string, string, T];

interface IRecord<T> {
  tag: "record";
  One: number;
  Two: boolean;
  Three: D<T>;
}
```

It's important to make sure your JSON and TypeScript are being derived with the same options. For this reason, we
include the convenience 'HasJSONOptions' typeclass, which lets you write the options only once, like this:

```haskell
instance HasJSONOptions MyType where getJSONOptions _ = (defaultOptions {fieldLabelModifier = drop 4})

$(deriveJSON (getJSONOptions (Proxy :: Proxy MyType)) ''MyType)
$(deriveTypeScript (getJSONOptions (Proxy :: Proxy MyType)) ''MyType)
```
