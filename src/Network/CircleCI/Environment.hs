{-|
Module      : Network.CircleCI.Environment
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

API calls for work with Environment Variables used in project build.

For more info please see "Environment variables" section in your CircleCI project's Settings.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Network.CircleCI.Environment (
    -- * API calls
      getEnvVars
    , getEnvVar
    , addEnvVar
    , deleteEnvVar
    -- * Types for calls and responses
    , EnvVar (..)
    , EnvVarDeleted (..)
    , EnvVarName
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
import qualified Data.Proxy                     as P
import           Data.Text                      ( Text )
import           Network.HTTP.Client            ( Manager )

import           Servant.API
import           Servant.Client

-- | Name of environment variable.
type EnvVarName = Text

-- | Shows list of environment variables for the single project. Based on https://circleci.com/docs/api/#list-environment-variables.
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
-- main = runCircleCI (getEnvVars $ ProjectPoint "denisshevchenko" "circlehs")
--                    (AccountAPIToken "e64c674195b87d76e988e9fbcba2whatever")
--     >>= \\case
--         Left problem  -> print problem
--         Right envVars -> print envVars
-- @
getEnvVars :: ProjectPoint              -- ^ Names of GitHub user/project.
           -> CircleCIResponse [EnvVar] -- ^ List of environment variables.
getEnvVars project = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantGetEnvVars (userName project)
                          (projectName project)
                          (Just token)
                          manager
                          apiBaseUrl

-- | Shows single environment variable. Based on https://circleci.com/docs/api/#get-environment-variable.
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
-- main = runCircleCI (getEnvVar project \"GCC\") token
--     >>= \\case
--         Left problem -> print problem
--         Right envVar -> print envVar
--   where
--     project = ProjectPoint "denisshevchenko" "circlehs"
--     token   = AccountAPIToken "e64c674195b87d76e988e9fbcba2whatever"
-- @
getEnvVar :: ProjectPoint            -- ^ Names of GitHub user/project.
          -> EnvVarName              -- ^ Environment variable name.
          -> CircleCIResponse EnvVar -- ^ Environment variable.
getEnvVar project envVarName = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantGetEnvVar (userName project)
                         (projectName project)
                         envVarName
                         (Just token)
                         manager
                         apiBaseUrl

-- | Adds environment variable. Based on https://circleci.com/docs/api/#add-environment-variable.
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
-- main = runCircleCI (addEnvVar project envVar) token
--     >>= \\case
--         Left problem    -> print problem
--         Right newEnvVar -> print newEnvVar
--   where
--     project = ProjectPoint "denisshevchenko" "circlehs"
--     envVar  = EnvVar \"GCC\" "\/usr\/local\/bin\/gcc-4.8"
--     token   = AccountAPIToken "e64c674195b87d76e988e9fbcba2whatever"
-- @
addEnvVar :: ProjectPoint            -- ^ Names of GitHub user/project.
          -> EnvVar                  -- ^ Environment variable.
          -> CircleCIResponse EnvVar -- ^ Added environment variable.
addEnvVar project envVar = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantAddEnvVar (userName project)
                         (projectName project)
                         (Just token)
                         envVar
                         manager
                         apiBaseUrl

-- | Deletes single environment variable. Based on https://circleci.com/docs/api/#delete-environment-variable.
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
-- main = runCircleCI (deleteEnvVar project \"GCC\") token
--     >>= \\case
--         Left problem    -> print problem
--         Right isDeleted -> print isDeleted
--   where
--     project = ProjectPoint "denisshevchenko" "circlehs"
--     token   = AccountAPIToken "e64c674195b87d76e988e9fbcba2whatever"
-- @
deleteEnvVar :: ProjectPoint                   -- ^ Names of GitHub user/project.
             -> EnvVarName                     -- ^ Environment variable name.
             -> CircleCIResponse EnvVarDeleted -- ^ Info about environment variable deleting.
deleteEnvVar project envVarName = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantDeleteEnvVar (userName project)
                            (projectName project)
                            envVarName
                            (Just token)
                            manager
                            apiBaseUrl

-- | Environment variable, name/value.
data EnvVar = EnvVar {
      name  :: Text
    , value :: Text
    } deriving (Eq, Show)

-- How to make EnvVar from JSON, for POST call.
instance FromJSON EnvVar where
    parseJSON (Object o) = EnvVar
        <$> o .: "name"
        <*> o .: "value"
    parseJSON _ = mzero

-- How to serialize EnvVar to JSON.
instance ToJSON EnvVar where
    toJSON (EnvVar aName aValue) =
        object [ "name"  .= aName
               , "value" .= aValue
               ]

-- | Environment variable deleting status.
data EnvVarDeleted = EnvVarSuccessfullyDeleted
                   | UnableToDeleteEnvVar ErrorMessage
                   deriving (Show)

-- How to create EnvVarDeleted from JSON.
instance FromJSON EnvVarDeleted where
    parseJSON (Object o) =
        o .: "message" >>= toEnvVarDeleted
    parseJSON _ = mzero

toEnvVarDeleted :: Text -> Parser EnvVarDeleted
toEnvVarDeleted rawMessage = return $
    if | rawMessage == "ok" -> EnvVarSuccessfullyDeleted
       | otherwise -> UnableToDeleteEnvVar rawMessage

-------------------------------------------------------------------------------
-- API types for Servant ------------------------------------------------------
-------------------------------------------------------------------------------

-- Complete API for work with environment variables.
type EnvVarAPI =
         GetEnvVarsCall
    :<|> GetEnvVarCall
    :<|> AddEnvVarCall
    :<|> DeleteEnvVarCall

-- Lists environment variables for project.
type GetEnvVarsCall =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> "envvar"
    :> QueryParam "circle-token" Token
    :> Get '[JSON] [EnvVar]
    -- GET: /project/:username/:project/envvar?circle-token=:token

-- Get single environment variable.
type GetEnvVarCall =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> "envvar"
    :> Capture "name" EnvVarName
    :> QueryParam "circle-token" Token
    :> Get '[JSON] EnvVar
    -- GET: /project/:username/:project/envvar/:name?circle-token=:token

-- Add environment variable.
type AddEnvVarCall =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> "envvar"
    :> QueryParam "circle-token" Token
    :> ReqBody '[JSON] EnvVar
    :> Post '[JSON] EnvVar
    -- POST: /project/:username/:project/envvar?circle-token=:token
    -- --header "Content-Type: application/json" -d '{"name":"foo", "value":"bar"}'

-- Delete environment variable.
type DeleteEnvVarCall =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> "envvar"
    :> Capture "name" EnvVarName
    :> QueryParam "circle-token" Token
    :> Delete '[JSON] EnvVarDeleted
    -- DELETE: /project/:username/:project/envvar/:name?circle-token=:token

-------------------------------------------------------------------------------
-- API client calls for Servant -----------------------------------------------
-------------------------------------------------------------------------------

servantGetEnvVars :: UserName
                  -> ProjectName
                  -> Maybe Token
                  -> Manager
                  -> BaseUrl
                  -> ClientM [EnvVar]

servantGetEnvVar :: UserName
                 -> ProjectName
                 -> EnvVarName
                 -> Maybe Token
                 -> Manager
                 -> BaseUrl
                 -> ClientM EnvVar

servantAddEnvVar :: UserName
                 -> ProjectName
                 -> Maybe Token
                 -> EnvVar
                 -> Manager
                 -> BaseUrl
                 -> ClientM EnvVar

servantDeleteEnvVar :: UserName
                    -> ProjectName
                    -> EnvVarName
                    -> Maybe Token
                    -> Manager
                    -> BaseUrl
                    -> ClientM EnvVarDeleted

servantGetEnvVars
 :<|> servantGetEnvVar
 :<|> servantAddEnvVar
 :<|> servantDeleteEnvVar = client envVarAPI

envVarAPI :: P.Proxy EnvVarAPI
envVarAPI = P.Proxy

