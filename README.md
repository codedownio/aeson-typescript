
# Welcome to `aeson-typescript` [![Hackage](https://img.shields.io/hackage/v/aeson-typescript.svg)](https://hackage.haskell.org/package/aeson-typescript) ![aeson-typescript](https://github.com/codedownio/aeson-typescript/workflows/aeson-typescript/badge.svg)

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
                  -- | This docstring will go into the generated TypeScript!
                  , testThree :: D a
                  } deriving Eq
```

Next we derive the necessary instances.

```haskell
$(deriveTypeScript (defaultOptions {fieldLabelModifier = drop 4, constructorTagModifier = map toLower}) ''D)
```

Now we can use the newly created instances.

```haskell
>>> putStrLn $ formatTSDeclarations $ getTypeScriptDeclarations (Proxy :: Proxy (D T))

type D<T> = "nullary" | IUnary<T> | IProduct<T> | IRecord<T>;

type IUnary<T> = number;

type IProduct<T> = [string, string, T];

interface IRecord<T> {
  tag: "record";
  One: number;
  Two: boolean;
  // This docstring will go into the generated TypeScript!
  Three: D<T>;
}
```

It's important to make sure your JSON and TypeScript are being derived with the same options. For this reason, we
include the convenience `HasJSONOptions` typeclass, which lets you write the options only once, like this:

```haskell
instance HasJSONOptions MyType where getJSONOptions _ = (defaultOptions {fieldLabelModifier = drop 4})

$(deriveJSON (getJSONOptions (Proxy :: Proxy MyType)) ''MyType)
$(deriveTypeScript (getJSONOptions (Proxy :: Proxy MyType)) ''MyType)
```

Or, if you want to be even more concise and don't mind defining the instances in the same file,

```haskell
myOptions = defaultOptions {fieldLabelModifier = drop 4}

$(deriveJSONAndTypeScript myOptions ''MyType)
```

Remembering that the Template Haskell `Q` monad is an ordinary monad, you can derive instances for several types at once like this:

```haskell
$(mconcat <$> traverse (deriveJSONAndTypeScript myOptions) [''MyType1, ''MyType2, ''MyType3])
```


# Suggestions for use

This library was written to make it easier to typecheck your TypeScript frontend against your Haskell backend. Here's how I like to integrate it into my workflow:

The idea is to set up a separate Haskell executable in your Cabal file whose sole purpose is to generate types. For example, in your hpack `package.yaml` file add a new executable like this:

```yaml
executables:
  ...
  tsdef:
    main: Main.hs
    source-dirs: tsdef
    dependencies:
    - my-main-app
    ...
```

And `tsdef/Main.hs` should look like this:

```haskell
module Main where

import Data.Proxy
import Data.Monoid
import MyLibraries

$(deriveTypeScript (getJSONOptions (Proxy :: Proxy MyType1)) ''MyType1)
$(deriveTypeScript (getJSONOptions (Proxy :: Proxy MyType2)) ''MyType2)
...

main = putStrLn $ formatTSDeclarations (
  (getTypeScriptDeclaration (Proxy :: Proxy MyType1)) <>
  (getTypeScriptDeclaration (Proxy :: Proxy MyType2)) <>
  ...
)
```

Now you can generate the types by running `stack runhaskell tsdef/Main.hs > types.d.ts`. I like to make this an automatic step in my Gulpfile, Webpack config, etc.


# See also

If you want a more opinionated web framework for generating APIs, check out [servant](http://haskell-servant.readthedocs.io/en/stable/). If you use Servant, you may enjoy [servant-typescript](https://github.com/codedownio/servant-typescript), which is based on `aeson-typescript`! This companion package also has the advantage of magically collecting all the types used in your API, so you don't have to list them out manually.

For another very powerful framework that can generate TypeScript client code based on an API specification, see [Swagger/OpenAPI](https://github.com/swagger-api/swagger-codegen).
