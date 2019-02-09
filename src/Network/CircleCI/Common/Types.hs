{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Network.CircleCI.Common.Types
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

Common types for work with CircleCI API.
-}

module Network.CircleCI.Common.Types (
      AccountAPIToken (..)
    , Token
    , UserName
    , ProjectName
    , BranchName
    , BuildNumber (..)
    , Email
    , CircleCIResponse
    , ProjectPoint (..)
    , ErrorMessage
) where

import           Servant.Client
import           Servant.API
import           Data.Aeson
import           Data.Text                ( Text )
import           Control.Monad.Reader

-- import CircleCI.Common.Run

-- | CircleCI account API token. List of account API tokens can be found at https://circleci.com/account/api.
newtype AccountAPIToken = AccountAPIToken Token

-- | API token as text, for Servant.
type Token = Text

-- | GitHub user name.
type UserName = Text

-- | GitHub project name.
type ProjectName = Text

-- | GitHub branch name.
type BranchName = Text

-- | Number of project's build on CircleCI.
newtype BuildNumber = BuildNumber Int
                    deriving (Eq, Show, ToHttpApiData, FromJSON, ToJSON)

-- | User email address.
type Email = Text

-- | Monad for response from CircleCI.
type CircleCIResponse a = ReaderT AccountAPIToken IO (Either ServantError a)

-- | GitHub project identifier, composed from user name and project name.
data ProjectPoint = ProjectPoint
    { userName    :: UserName     -- ^ GitHub user name.
    , projectName :: ProjectName  -- ^ GitHub project name.
    }

-- | Message about some problem.
type ErrorMessage = Text

