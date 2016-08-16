# Installation

## Dependencies

Smokehill is a Haskell project. To build and install it you need:

1. ghc
2. cabal
3. haskell
4. stack

To use Smokehill you need the following programs installed

1. idris
2. git
3. hg -- only if your packages use HG.

## Build instructions

To build Smokehill you can either:

### Use stack

In the project's root directory run the command:

```sh

stack install
```

This will install an executable into `$HOME/.local/bin/`.

### Use cabal

In the project's root directory run the command:

```sh
cabal configure && cabal install
```

This will install an executable into `$HOME/.cabal/bin`.

### Use cabal sandbox

In the project's root directory run the command:

```sh
cabal configure && cabal sandbox init && cabal install
```

This will install an executable into `$PROJECTROOT/.cabal-sandbox/.../bin/`.
