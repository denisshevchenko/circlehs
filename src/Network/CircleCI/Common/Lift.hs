{-|
Module      : Network.CircleCI.Common.Lift
Copyright   : (c) Ben Gamari, 2018
License     : MIT
Maintainer  : ben@well-typed.com
Stability   : alpha

Lift @servant-client@ computations into the 'CircleCIResponse' monad.
-}

module Network.CircleCI.Common.Lift (
    liftClientM
) where

import           Servant.Client hiding (manager)
import           Control.Monad.Reader
import           Network.CircleCI.Common.HTTPS
import           Network.CircleCI.Common.Types
import           Network.CircleCI.Common.URL

-- | Lift a 'ClientM' computation into the 'CircleCIResponse' monad.
liftClientM :: ClientM a -> CircleCIResponse a
liftClientM action = do
    manager <- httpsManager
    let env = mkClientEnv manager apiBaseUrl
    liftIO $ runClientM action env
