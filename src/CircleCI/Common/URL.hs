{-|
Module      : CircleCI.Common.URL
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

Base URL for all CircleCI API calls. Based on https://circleci.com/docs/api/#calling.
-}

module CircleCI.Common.URL (
    apiBaseUrl
) where

import Servant.Common.BaseUrl

-- | Base URL for all API calls. For API v1 it's @https://circleci.com/api/v1/@.
apiBaseUrl :: BaseUrl
apiBaseUrl = BaseUrl {
      baseUrlScheme = Https
    , baseUrlHost   = "circleci.com"
    , baseUrlPort   = 443
    , baseUrlPath   = "/api/v1"
    }

