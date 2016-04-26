{-|
Module      : CircleCI.User
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

API calls for work with User info.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module CircleCI.User (
    -- * API call
      getUserInfo
    -- * Types for calls and response
    , UserInfo (..)
    , ProjectShortInfo (..)
    , EmailNotification (..)
    , Plan (..)
    , GitHubOAuth (..)
    , AnalyticsId
    , module CircleCI.Common.Types
    , module CircleCI.Common.Run
) where

import           CircleCI.Common.URL
import           CircleCI.Common.Types
import           CircleCI.Common.HTTPS
import           CircleCI.Common.Run

import           Control.Monad                  ( mzero )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.Reader           ( ask )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson
import           Data.Aeson.Types
import           Data.HashMap.Strict
import qualified Data.Proxy                     as P
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Network.HTTP.Client            ( Manager )

import           Servant.API
import           Servant.Client

-- | Show info about user. Based on https://circleci.com/docs/api/#user.
--
-- Usage example:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- {-\# LANGUAGE LambdaCase \#-}
--
-- import CircleCI
--
-- main :: IO ()
-- main = runCircleCI getUserInfo
--                    (AccountAPIToken "e64c674195bbc0d0be3efa2whatever")
--     >>= \\case
--         Left problem -> print problem
--         Right info   -> print info
-- @
getUserInfo :: CircleCIResponse UserInfo -- ^ Info about the signed in user.
getUserInfo = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantGetUserInfo (Just token)
                           manager
                           apiBaseUrl

-- | User's analytics id. For example, @"6fc20e13-008e-4dc9-b158-ababd33a099d"@.
type AnalyticsId = Text

-- | Info about user.
data UserInfo = UserInfo {
      inBetaProgram         :: Bool                 -- ^ Does user in the CircleCI Beta Program?
    , fullName              :: Text                 -- ^ User's full name.
    , allEmails             :: [Text]               -- ^ All email addresses in user's account.
    , defaultEmail          :: Text                 -- ^ Default email for notifications.
    , basicEmailPreference  :: EmailNotification    -- ^ User email notifications.
    , gitHubAvatarUrl       :: Text                 -- ^ GitHub avatar URL.
    , gitHubId              :: Integer              -- ^ GitHub ID.
    , gitHubLogin           :: Text                 -- ^ User's GitHub login.
    , gitHubOAuthScopes     :: [GitHubOAuth]        -- ^ GitHub OAuth scopes.
    , accountCreatedAt      :: UTCTime              -- ^ Date when CircleCI-account was created.
    , trialEndDate          :: UTCTime              -- ^ Trial period end date.
    , plan                  :: Plan                 -- ^ User's plan pricing.
    , parallelism           :: Int                  -- ^ Parallelism for tests.
    , containtersNumber     :: Int                  -- ^ Number of user's build containers.
    , projects              :: [ProjectShortInfo]   -- ^ User's projects, short info.
    , analyticsId           :: AnalyticsId          -- ^ Analytics ID.
    , pusherId              :: Text                 -- ^ Pusher ID.
    , herokuAPIKey          :: Maybe Text           -- ^ Heroku API key.
    } deriving (Show)

-- How to create UserInfo from JSON.
instance FromJSON UserInfo where
    parseJSON (Object o) = UserInfo
        <$>  o .:  "in_beta_program"
        <*>  o .:  "name"
        <*>  o .:  "all_emails"
        <*>  o .:  "selected_email"
        <*> (o .:  "basic_email_prefs"     >>= toEmailPreference)
        <*>  o .:  "avatar_url"
        <*>  o .:  "github_id"
        <*>  o .:  "login"
        <*> (o .:  "github_oauth_scopes"   >>= toGitHubOAuth)
        <*>  o .:  "created_at"
        <*>  o .:  "trial_end"
        <*> (o .:? "plan"           .!= "" >>= toPlan)
        <*>  o .:  "parallelism"
        <*>  o .:  "containers"
        <*> (o .:  "projects"              >>= toProjectsShortInfo)
        <*>  o .:  "analytics_id"
        <*>  o .:  "pusher_id"
        <*>  o .:? "heroku_api_key"
    parseJSON _ = mzero

-- | Short info about the project.
data ProjectShortInfo = ProjectShortInfo {
      gitHubURL         :: Text                 -- ^ Project's GitHUb URL.
    , onDashboard       :: Bool                 -- ^ Does this project on CircleCI Dashboard?
    , emailNotification :: EmailNotification    -- ^ Email notifications for this project.
    } deriving (Show)

toProjectsShortInfo :: HashMap Text RawProject -> Parser [ProjectShortInfo]
toProjectsShortInfo rawProjects = return
    [ProjectShortInfo { gitHubURL         = githubUrl
                      , onDashboard       = rawOnDashboard rawProject
                      , emailNotification = rawEmailNotification rawProject
                      }
    | (githubUrl, rawProject) <- toList rawProjects]

data RawProject = RawProject {
      rawOnDashboard       :: Bool
    , rawEmailNotification :: EmailNotification
    } deriving (Show)

instance FromJSON RawProject where
    parseJSON (Object o) = RawProject
        <$> o  .: "on_dashboard"
        <*> (o .: "emails" >>= toEmailPreference)
    parseJSON _ = mzero

-- | CircleCI plan. For more info please see https://circleci.com/pricing/.
data Plan = Hobbyist
            | Plan Text
            deriving (Show)

toPlan :: Text -> Parser Plan
toPlan ""  = return Hobbyist
toPlan key = return $ Plan key

-- | Email notification preference.
data EmailNotification = DefaultNotification
                       | AllBuildsNotification
                       | BranchesIHavePushedTo
                       | WithoutNotification
                       deriving (Show)

toEmailPreference :: Text -> Parser EmailNotification
toEmailPreference "default" = return DefaultNotification
toEmailPreference "all"     = return AllBuildsNotification
toEmailPreference "smart"   = return BranchesIHavePushedTo
toEmailPreference "none"    = return WithoutNotification
toEmailPreference _         = return DefaultNotification

-- | GitHub OAuth mode.
data GitHubOAuth = UserEmailOAuth
                 | RepoOAuth
                 deriving (Show)

toGitHubOAuth :: [Text] -> Parser [GitHubOAuth]
toGitHubOAuth raw = return $ Prelude.map convert raw
  where
    convert :: Text -> GitHubOAuth
    convert "user:email" = UserEmailOAuth
    convert "repo"       = RepoOAuth
    convert _            = RepoOAuth

-------------------------------------------------------------------------------
-- API types for Servant ------------------------------------------------------
-------------------------------------------------------------------------------

-- Complete API for work with user info.
type UserAPI = GetUserInfoCall

-- Clears build cache.
type GetUserInfoCall =
       "me"
    :> QueryParam "circle-token" Token
    :> Get '[JSON] UserInfo
    -- GET: /me?circle-token=:token

-------------------------------------------------------------------------------
-- API client calls for Servant -----------------------------------------------
-------------------------------------------------------------------------------

servantGetUserInfo :: Maybe Token
                   -> Manager
                   -> BaseUrl
                   -> ClientM UserInfo
servantGetUserInfo = client userAPI

userAPI :: P.Proxy UserAPI
userAPI = P.Proxy

