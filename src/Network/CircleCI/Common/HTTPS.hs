{-|
Module      : Network.CircleCI.Common.HTTPS
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

Connection manager for @https@-requests. CircleCI uses TLS.
-}

module Network.CircleCI.Common.HTTPS (
    httpsManager
) where

import           Control.Monad.IO.Class
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS

-- | Connection manager with TLS support.
httpsManager :: MonadIO m => m Manager
httpsManager = liftIO $ newManager tlsManagerSettings

