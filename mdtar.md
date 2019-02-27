## mdtar.cabal

```cabal
cabal-version: 2.4

name: mdtar
version: 0

library
    hs-source-dirs: library
    exposed-modules: Text.MarkdownTAR
    build-depends: base, megaparsec

executable mdtar
    hs-source-dirs: executables
    main-is: mdtar.hs
    build-depends: base, mdtar, optparse-applicative
```

## library/Text/MarkdownTar.hs

```haskell
module Text.MarkdownTAR where


```

## executables/mdtar.hs

```haskell
module Main where

import Text.MarkdownTAR
```
