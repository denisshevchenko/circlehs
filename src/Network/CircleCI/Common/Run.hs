{-|
Module      : Network.CircleCI.Common.Run
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

Run 'CircleCIResponse' monad.
-}

module Network.CircleCI.Common.Run (
    runCircleCI
) where

import           Control.Monad.Reader

-- | All API calls require account API token, so move it into 'ReaderT' monad
-- instead of passing it as an explicit argument.
runCircleCI :: ReaderT t IO a -> t -> IO a
runCircleCI = runReaderT

