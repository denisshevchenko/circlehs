{-|
Module      : Network.CircleCI.Build
Copyright   : (c) Ben Gamari, 2018
License     : MIT
Maintainer  : ben@well-typed.com
Stability   : alpha
API calls for triggering and querying builds.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Network.CircleCI.Build (
    -- * API calls
      triggerBuild
    , getBuild
    -- * Types for calls and response
    -- ** Querying
    , BuildInfo(..)
    , BuildOutcome(..)
    , BuildLifecycle(..)
    -- ** Triggering
    , TagName
    , Revision
    , TriggerBuildOptions(..)
    , BuildTarget(..)
    -- ** General
    , module Network.CircleCI.Common.Types
    , module Network.CircleCI.Common.Run
) where

import           Network.CircleCI.Common.Lift
import           Network.CircleCI.Common.Types
import           Network.CircleCI.Common.Run

import           Control.Monad                  ( mzero )
import           Control.Monad.Reader           ( ask )
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Proxy                     as P
import           Data.Text                      ( Text )
import           Data.HashMap.Strict            ( HashMap )

import           Servant.API
import           Servant.Client

-- | The name of a git tag.
type TagName = Text

-- | A git commit SHA.
type Revision = Text

-- | Trigger a build of a tag. Based on https://circleci.com/docs/api/v1-reference/#new-build.
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
-- main = runCircleCI (triggerBuildTag (ProjectPoint "denisshevchenko" "circlehs") "a-tag")
--                    (AccountAPIToken "e64c674195bbc0d0be3efa2whatever")
--     >>= \\case
--         Left problem  -> print problem
--         Right buildNo -> print buildNo
-- @
triggerBuild :: ProjectPoint -> TriggerBuildOptions -> CircleCIResponse BuildInfo
triggerBuild project opts = do
    AccountAPIToken token <- ask
    liftClientM $ servantTriggerBuild
        (userName project)
        (projectName project)
        (Just token)
        opts

-- | Get information about a build.
getBuild :: ProjectPoint -> BuildNumber -> CircleCIResponse BuildInfo
getBuild project build = do
    AccountAPIToken token <- ask
    liftClientM $ servantGetBuild
        (userName project)
        (projectName project)
        build
        (Just token)

-------------------------------------------------------------------------------
-- API types for Servant ------------------------------------------------------
-------------------------------------------------------------------------------

-- Complete API to trigger builds
type BuildAPI =
         TriggerBuild
    :<|> GetBuild

-- Trigger a build.
type TriggerBuild =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> QueryParam "circle-token" Token
    :> ReqBody '[JSON] TriggerBuildOptions
    :> Post '[JSON] BuildInfo
    -- POST: /project/:username/:project?circle-token=:token

data TriggerBuildOptions = TriggerBuildOptions {
      triggerBuildTarget :: BuildTarget
    , triggerBuildParams :: HashMap Text Text
    }

instance ToJSON TriggerBuildOptions where
    toJSON opts =
        object $ [ "build_parameters" .= triggerBuildParams opts ]
              ++ target
      where
        target =
            case triggerBuildTarget opts of
              BuildRevision rev -> [ "revision" .= rev ]
              BuildTag tag      -> [ "tag" .= tag ]
              BuildHead         -> []

-- | What should be built?
data BuildTarget = BuildRevision Revision
                   -- ^ build a particular git revision
                 | BuildTag TagName
                   -- ^ build a particular git tag
                 | BuildHead
                   -- ^ build the head of the default branch

-- Get information about a build
type GetBuild =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> Capture "build number" BuildNumber
    :> QueryParam "circle-token" Token
    :> Get '[JSON] BuildInfo
    -- GET: /project/:username/:project/:build_num?circle-token=:tokenh

-- | Info about single build.
data BuildInfo = BuildInfo {
      outcome     :: Maybe BuildOutcome
    , lifecycle   :: BuildLifecycle
    , number      :: BuildNumber
    , commit      :: Text
    } deriving (Eq, Show)

-- How we create BuildInfo from JSON.
instance FromJSON BuildInfo where
    parseJSON (Object o) = BuildInfo
        <$> (o .:? "outcome" >>= traverse toBuildOutcome)
        <*> (o .: "lifecycle" >>= toBuildLifecycle)
        <*>  o .: "build_num"
        <*>  o .: "vcs_revision"
    parseJSON _ = mzero

toBuildOutcome :: Text -> Parser BuildOutcome
toBuildOutcome "success"             = return BuildSuccess
toBuildOutcome "failed"              = return BuildFailed
toBuildOutcome "canceled"            = return BuildCanceled
toBuildOutcome "no_tests"            = return BuildNoTests
toBuildOutcome "infrastructure_fail" = return BuildInfrastructureFail
toBuildOutcome "timedout"            = return BuildTimedOut
toBuildOutcome _                     = fail "unknown build outcome"

-- | Build outcome.
data BuildOutcome = BuildSuccess
                  | BuildFailed
                  | BuildCanceled
                  | BuildNoTests
                  | BuildInfrastructureFail
                  | BuildTimedOut
                  deriving (Eq, Show)

toBuildLifecycle :: Text -> Parser BuildLifecycle
toBuildLifecycle "queued"      = return BuildQueued
toBuildLifecycle "scheduled"   = return BuildScheduled
toBuildLifecycle "not_run"     = return BuildNotRun
toBuildLifecycle "not_running" = return BuildNotRunning
toBuildLifecycle "running"     = return BuildRunning
toBuildLifecycle "finished"    = return BuildFinished
toBuildLifecycle _             = fail "unknown build lifecycle"

-- | Build lifecycle.
data BuildLifecycle = BuildQueued
                    | BuildScheduled
                    | BuildNotRun
                    | BuildNotRunning
                    | BuildRunning
                    | BuildFinished
                    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- API client calls for Servant -----------------------------------------------
-------------------------------------------------------------------------------

servantTriggerBuild :: UserName
                    -> ProjectName
                    -> Maybe Token
                    -> TriggerBuildOptions
                    -> ClientM BuildInfo

servantGetBuild :: UserName
                -> ProjectName
                -> BuildNumber
                -> Maybe Token
                -> ClientM BuildInfo

servantTriggerBuild
    :<|> servantGetBuild = client buildAPI

buildAPI :: P.Proxy BuildAPI
buildAPI = P.Proxy
