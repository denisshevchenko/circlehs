[![CircleCI](https://circleci.com/gh/denisshevchenko/circlehs.svg?style=shield&circle-token=6a0ecfd0e019941c9da7ed3070d6cbaa29e3597d)](https://circleci.com/gh/denisshevchenko/circlehs)

# CircleHs

The [CircleCI](https://circleci.com/) REST API implementation in Haskell. For more info please see [official API reference](https://circleci.com/docs/api/).

**IMPORTANT!** THE IMPLEMENTATION IS INCOMPLETE AND UNSTABLE, WORK IN PROGRESS.

## Hello, CircleCI!

Let's obtain information about the user:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import CircleHs

main :: IO ()
main = runCircleCI getUserInfo
                   (AccountAPIToken "e64c67410f96ba2whatever")
    >>= \case
        Left problem -> print problem
        Right info   -> print info
```

## Install

Add `circlehs` into `build-depends`-section in your `.cabal`-file. Then choose your way.

### With stack

From GitHub - add this in your `stack.yaml`:

```yaml
- location:
    git: https://github.com/denisshevchenko/circlehs.git
    commit: 0ef96c83b8c237dc3b19c744642bc6whatever
```

Then:

```bash
$ stack build
```

### With cabal

Soon. By the way, `stack`-way is much better... ;-)

