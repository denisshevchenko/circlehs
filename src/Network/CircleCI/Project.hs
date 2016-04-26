{-|
Module      : Network.CircleCI.Project
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

API calls for work with info about projects.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Network.CircleCI.Project (
    -- * API call
      getProjectsInfo
    -- * Types for calls and response
    , ProjectInfo (..)
    , BranchBuildInfo (..)
    , BuildInfo (..)
    , BuildStatus (..)
    , module Network.CircleCI.Common.Types
    , module Network.CircleCI.Common.Run
) where

import           Network.CircleCI.Common.URL
import           Network.CircleCI.Common.Types
import           Network.CircleCI.Common.HTTPS
import           Network.CircleCI.Common.Run

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

-- | Show info about all projects user is following. Based on https://circleci.com/docs/api/#projects.
--
-- Usage example:
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- {-\# LANGUAGE LambdaCase \#-}
--
-- import Network.CircleCI
--
-- main :: IO ()
-- main = runCircleCI getProjectsInfo
--                    (AccountAPIToken "e64c674195bbc0d0be3efa2whatever")
--     >>= \\case
--         Left problem -> print problem
--         Right info   -> print info
-- @
getProjectsInfo :: CircleCIResponse [ProjectInfo] -- ^ Info about projects.
getProjectsInfo = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantGetProjectsInfo (Just token)
                               manager
                               apiBaseUrl

-- | Info about single project.
data ProjectInfo = ProjectInfo {
      language                  :: Text                 -- ^ Programming language using in this project.
    , repositoryName            :: Text                 -- ^
    , repositoryUrl             :: Text                 -- ^ Repository URL.
    , branches                  :: [BranchBuildInfo]    -- ^ Info about recent builds in branches.
    , defaultBranch             :: Text                 -- ^ Name of default branch.
    , isOpenSource              :: Bool                 -- ^ If 'True' - project is open.
    , followed                  :: Bool                 -- ^ If 'True' - project is followed by some user.
    , gitHubUserName            :: Text                 -- ^ GitHub user name.
    -- Slack
    , slackChannel              :: Maybe Text           -- ^ Slack channel name.
    , slackWebhookUrl           :: Maybe Text           -- ^ Slack webhook URL.
    , slackNotifyPreferenses    :: Maybe Text           -- ^ Slack notify preferences.
    , slackSubdomain            :: Maybe Text           -- ^ Slack subdomain.
    , slackAPIToken             :: Maybe Text           -- ^ Slack API token.
    -- HipChat
    , hipchatNotifyPreferenses  :: Maybe Text           -- ^ HipChat notify preferences.
    , hipchatNotify             :: Maybe Text           -- ^ HipChat notify.
    , hipchatAPIToken           :: Maybe Text           -- ^ HipChat API token.
    , hipchatRoom               :: Maybe Text           -- ^ HipChat room name.
    -- IRC
    , ircServer                 :: Maybe Text           -- ^ IRC server.
    , ircKeyword                :: Maybe Text           -- ^ IRC keyword.
    , ircChannel                :: Maybe Text           -- ^ IRC channel name.
    , ircUsername               :: Maybe Text           -- ^ IRC user name.
    , ircPassword               :: Maybe Text           -- ^ IRC password.
    , ircNotifyPreferenses      :: Maybe Text           -- ^ IRC notify preferences.
    } deriving (Show)

-- How to create ProjectInfo from JSON.
instance FromJSON ProjectInfo where
    parseJSON (Object o) = ProjectInfo
        <$>  o .:  "language"
        <*>  o .:  "reponame"
        <*>  o .:  "vcs_url"
        <*> (o .:  "branches" >>= toBranchesBuildInfo)
        <*>  o .:  "default_branch"
        <*>  o .:  "oss"
        <*>  o .:  "followed"
        <*>  o .:  "username"
        <*>  o .:? "slack_channel"
        <*>  o .:? "slack_webhook_url"
        <*>  o .:? "slack_notify_prefs"
        <*>  o .:? "slack_subdomain"
        <*>  o .:? "slack_api_token"
        <*>  o .:? "hipchat_notify_prefs"
        <*>  o .:? "hipchat_notify"
        <*>  o .:? "hipchat_api_token"
        <*>  o .:? "hipchat_room"
        <*>  o .:? "irc_server"
        <*>  o .:? "irc_keyword"
        <*>  o .:? "irc_channel"
        <*>  o .:? "irc_username"
        <*>  o .:? "irc_password"
        <*>  o .:? "irc_notify_prefs"
    parseJSON _ = mzero

toBranchesBuildInfo :: HashMap Text RawBranchBuildInfo -> Parser [BranchBuildInfo]
toBranchesBuildInfo rawBranchesBuildInfo = return
    [BranchBuildInfo { branchName       = aBranchName
                     , lastSuccessBuild = rawLastSuccessBuild   rawBranchesBuild
                     , lastFailedBuild  = rawLastFailedBuild    rawBranchesBuild
                     , pusherLogins     = rawPusherLogins       rawBranchesBuild
                     , recentBuilds     = rawRecentBuilds       rawBranchesBuild
                     , runningBuilds    = rawRunningBuilds      rawBranchesBuild
                     }
    | (aBranchName, rawBranchesBuild) <- toList rawBranchesBuildInfo]

-- Raw build info for a single branch, for HashMap.
data RawBranchBuildInfo = RawBranchBuildInfo {
      rawLastSuccessBuild  :: Maybe BuildInfo
    , rawLastFailedBuild   :: Maybe BuildInfo
    , rawPusherLogins      :: [Text]
    , rawRecentBuilds      :: Maybe [BuildInfo]
    , rawRunningBuilds     :: Maybe [BuildInfo]
    } deriving (Eq, Show)

-- How we create RawBranchBuildInfo from raw JSON.
instance FromJSON RawBranchBuildInfo where
    parseJSON (Object o) = RawBranchBuildInfo
        <$> o .:? "last_success"
        <*> o .:? "last_non_success"
        <*> o .:  "pusher_logins"
        <*> o .:? "recent_builds"
        <*> o .:? "running_builds"
    parseJSON _ = mzero

-- | Build info for a single branch.
data BranchBuildInfo = BranchBuildInfo {
      branchName        :: Text
    , lastSuccessBuild  :: Maybe BuildInfo
    , lastFailedBuild   :: Maybe BuildInfo
    , pusherLogins      :: [Text]
    , recentBuilds      :: Maybe [BuildInfo]
    , runningBuilds     :: Maybe [BuildInfo]
    } deriving (Eq, Show)

-- | Info about single build.
data BuildInfo = BuildInfo {
      status        :: BuildStatus
    , number        :: Int
    , commit        :: Text
    , pushDate      :: UTCTime
    , addingDate    :: UTCTime
    } deriving (Eq, Show)

-- How we create BuildInfo from raw JSON.
instance FromJSON BuildInfo where
    parseJSON (Object o) = BuildInfo
        <$> (o .: "status" >>= toBuildStatus)
        <*>  o .: "build_num"
        <*>  o .: "vcs_revision"
        <*>  o .: "pushed_at"
        <*>  o .: "added_at"
    parseJSON _ = mzero

toBuildStatus :: Text -> Parser BuildStatus
toBuildStatus "running"  = return BuildRunning
toBuildStatus "success"  = return BuildSuccess
toBuildStatus "failed"   = return BuildFailed
toBuildStatus "canceled" = return BuildCanceled
toBuildStatus "no_tests" = return NoTests
toBuildStatus _          = return BuildSuccess

-- | Build status.
data BuildStatus = BuildRunning
                 | BuildSuccess
                 | BuildFailed
                 | BuildCanceled
                 | NoTests
                 deriving (Eq, Show)

-------------------------------------------------------------------------------
-- API types for Servant ------------------------------------------------------
-------------------------------------------------------------------------------

-- Complete API for work with projects.
type ProjectsInfoAPI = GetProjectsInfoCall

-- Obtain info about Projects.
type GetProjectsInfoCall =
       "projects"
    :> QueryParam "circle-token" Token
    :> Get '[JSON] [ProjectInfo]
    -- GET: /projects?circle-token=:token

-------------------------------------------------------------------------------
-- API client calls for Servant -----------------------------------------------
-------------------------------------------------------------------------------

servantGetProjectsInfo :: Maybe Token
                       -> Manager
                       -> BaseUrl
                       -> ClientM [ProjectInfo]
servantGetProjectsInfo = client projectsInfoAPI

projectsInfoAPI :: P.Proxy ProjectsInfoAPI
projectsInfoAPI = P.Proxy

