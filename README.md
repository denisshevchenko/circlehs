[![CircleCI](https://circleci.com/gh/denisshevchenko/circlehs.svg?style=shield&circle-token=6a0ecfd0e019941c9da7ed3070d6cbaa29e3597d)](https://circleci.com/gh/denisshevchenko/circlehs)&nbsp;&nbsp;&nbsp;[![Code Climate](https://codeclimate.com/github/denisshevchenko/circlehs/badges/gpa.svg)](https://codeclimate.com/github/denisshevchenko/circlehs)

# CircleHs

The [CircleCI](https://circleci.com/) REST API implementation in Haskell. For more info please see [official API reference](https://circleci.com/docs/api/).

**IMPORTANT!** THE IMPLEMENTATION IS INCOMPLETE AND UNSTABLE, WORK IN PROGRESS.

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

