[![CircleCI](https://circleci.com/gh/denisshevchenko/circlehs.svg?style=shield&circle-token=6a0ecfd0e019941c9da7ed3070d6cbaa29e3597d)](https://circleci.com/gh/denisshevchenko/circlehs)&nbsp;&nbsp;&nbsp;[![Code Climate](https://codeclimate.com/github/denisshevchenko/circlehs/badges/gpa.svg)](https://codeclimate.com/github/denisshevchenko/circlehs)&nbsp;&nbsp;&nbsp;[![Hackage](https://img.shields.io/badge/hackage-v0.0.3-blue.svg)](http://hackage.haskell.org/package/circlehs)

# CircleHs

The [CircleCI](https://circleci.com/) REST API implementation in Haskell. For more info please see [official API reference](https://circleci.com/docs/api/).

Work in progress.

## Hello, CircleCI!

Let's obtain information about the user:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Network.CircleCI

main :: IO ()
main = runCircleCI getUserInfo
                   (AccountAPIToken "e64c67410f96ba2whatever")
    >>= \case
        Left problem -> print problem
        Right info   -> print info
```

