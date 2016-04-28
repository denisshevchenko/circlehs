{-|
Module      : Network.CircleCI.CheckoutKey
Copyright   : (c) Denis Shevchenko, 2016
License     : MIT
Maintainer  : me@dshevchenko.biz
Stability   : alpha

API calls for work with Checkout Keys. CircleCI uses Checkout Keys to check out your GitHub project, submodules, and private dependencies.

For more info please see "Checkout SSH keys" section in your CircleCI project's Settings.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Network.CircleCI.CheckoutKey (
    -- * API calls
      getCheckoutKeys
    , getCheckoutKey
    , createCheckoutKey
    , deleteCheckoutKey
    -- * Types for calls and responses
    , Fingerprint (..)
    , CheckoutKeyInfo (..)
    , CheckoutKeyType (..)
    , CheckoutKeyDeleted (..)
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
import           Data.Time.Clock                ( UTCTime )
import           Network.HTTP.Client            ( Manager )

import           Servant.API
import           Servant.Client

-- | Shows list of checkout keys. Based on https://circleci.com/docs/api/#list-checkout-keys.
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
-- main = runCircleCI (getCheckoutKeys $ ProjectPoint "denisshevchenko" "circlehs")
--                    (AccountAPIToken "e64c674195bbc0d0be3ef9679b6c6ba2whatever")
--     >>= \\case
--         Left problem -> print problem
--         Right keys   -> print keys
-- @
getCheckoutKeys :: ProjectPoint                       -- ^ Names of GitHub user/project.
                -> CircleCIResponse [CheckoutKeyInfo] -- ^ List of checkout keys.
getCheckoutKeys project = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantGetCheckoutKeys (userName project)
                               (projectName project)
                               (Just token)
                               manager
                               apiBaseUrl

-- | Shows single checkout key. Based on https://circleci.com/docs/api/#get-checkout-key.
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
-- main = runCircleCI (getCheckoutKey project fingerprint) apiToken
--     >>= \\case
--         Left problem -> print problem
--         Right key    -> print key
--   where
--     project     = ProjectPoint "denisshevchenko" "circlehs"
--     fingerprint = Fingerprint "79:23:05:6a:6d:4c:3c:5c:0e:64:79:49:f0:e9:8d:a0"
--     apiToken    = AccountAPIToken "e64c674195bbc0d0be3ef9679b6c6ba2whatever"
-- @
getCheckoutKey :: ProjectPoint                      -- ^ Names of GitHub user/project.
               -> Fingerprint                       -- ^ Key fingerprint.
               -> CircleCIResponse CheckoutKeyInfo  -- ^ Checkout key info.
getCheckoutKey project (Fingerprint aFingerprint) = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantGetCheckoutKey (userName project)
                              (projectName project)
                              aFingerprint
                              (Just token)
                              manager
                              apiBaseUrl

-- | Creates checkout key. Based on https://circleci.com/docs/api/#new-checkout-key.
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
-- main = runCircleCI (createCheckoutKey $ ProjectPoint "denisshevchenko" "circlehs")
--                    (AccountAPIToken "e64c674195bbc0d0be3ef9679b6c6ba2whatever")
--     >>= \\case
--         Left problem -> print problem
--         Right newKey -> print newKey
-- @
createCheckoutKey :: ProjectPoint                     -- ^ Names of GitHub user/project.
                  -> CircleCIResponse CheckoutKeyInfo -- ^ New checkout key info.
createCheckoutKey project = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantCreateCheckoutKey (userName project)
                                 (projectName project)
                                 (Just token)
                                 manager
                                 apiBaseUrl

-- | Deletes single checkout key. Based on https://circleci.com/docs/api/#delete-checkout-key.
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
-- main = runCircleCI (deleteCheckoutKey project fingerprint) apiToken
--     >>= \\case
--         Left problem    -> print problem
--         Right isDeleted -> print isDeleted
--   where
--     project     = ProjectPoint "denisshevchenko" "circlehs"
--     fingerprint = Fingerprint "79:23:05:6a:6d:4c:3c:5c:0e:64:79:49:f0:e9:8d:a0"
--     apiToken    = AccountAPIToken "e64c674195bbc0d0be3ef9679b6c6ba2whatever"
-- @
deleteCheckoutKey :: ProjectPoint                         -- ^ Names of GitHub user/project.
                  -> Fingerprint                          -- ^ Key fingerprint.
                  -> CircleCIResponse CheckoutKeyDeleted  -- ^ Status of checkout key deletion.
deleteCheckoutKey project (Fingerprint aFingerprint) = do
    AccountAPIToken token <- ask
    liftIO . runExceptT $ do
        manager <- httpsManager
        servantDeleteCheckoutKey (userName project)
                                 (projectName project)
                                 aFingerprint
                                 (Just token)
                                 manager
                                 apiBaseUrl

-- | Checkout key fingerprint. For example, @"79:23:05:6a:6d:4c:3c:5c:0e:64:79:49:f0:e9:8d:a0"@.
newtype Fingerprint = Fingerprint Text
                    deriving (Eq, Show)

-- | Type of checkout key.
data CheckoutKeyType = GitHubDeployKey  -- ^ Repo-specific SSH key.
                     | GitHubUserKey    -- ^ User-specific SSH key.
                     deriving (Eq, Show)

-- | Info about checkout key.
data CheckoutKeyInfo = CheckoutKeyInfo {
      publicKey   :: Text            -- ^ Public SSH key.
    , keyType     :: CheckoutKeyType -- ^ Key type.
    , fingerprint :: Fingerprint     -- ^ Key fingerprint.
    , preferred   :: Bool            -- ^ Preferred key or not.
    , issueDate   :: UTCTime         -- ^ Date when this key was issued.
    } deriving (Eq, Show)

-- How to create CheckoutKeyInfo from JSON.
instance FromJSON CheckoutKeyInfo where
    parseJSON (Object o) = CheckoutKeyInfo
        <$>  o .: "public_key"
        <*> (o .: "type"        >>= toCheckoutKeyType)
        <*> (o .: "fingerprint" >>= toFingerprint)
        <*>  o .: "preferred"
        <*>  o .: "time"
    parseJSON _ = mzero

toCheckoutKeyType :: Text -> Parser CheckoutKeyType
toCheckoutKeyType "deploy-key"      = return GitHubDeployKey
toCheckoutKeyType "github-user-key" = return GitHubUserKey
toCheckoutKeyType _                 = return GitHubDeployKey

toFingerprint :: Text -> Parser Fingerprint
toFingerprint = return . Fingerprint

-- | Checkout key deleting status.
data CheckoutKeyDeleted = KeySuccessfullyDeleted
                        | UnableToDeleteKey ErrorMessage
                        deriving (Show)

-- How to create CheckoutKeyDeleted from JSON.
instance FromJSON CheckoutKeyDeleted where
    parseJSON (Object o) =
        o .: "message" >>= toCheckoutKeyDeleted
    parseJSON _ = mzero

toCheckoutKeyDeleted :: Text -> Parser CheckoutKeyDeleted
toCheckoutKeyDeleted "ok"       = return KeySuccessfullyDeleted
toCheckoutKeyDeleted rawMessage = return $ UnableToDeleteKey rawMessage

-------------------------------------------------------------------------------
-- API types for Servant ------------------------------------------------------
-------------------------------------------------------------------------------

-- Complete API for work with checkout keys.
type CheckoutKeyAPI =
         GetCheckoutKeysCall
    :<|> GetCheckoutKeyCall
    :<|> CreateCheckoutKeyCall
    :<|> DeleteCheckoutKeyCall

-- Lists checkout keys.
type GetCheckoutKeysCall =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> "checkout-key"
    :> QueryParam "circle-token" Token
    :> Get '[JSON] [CheckoutKeyInfo]
    -- GET: /project/:username/:project/checkout-key?circle-token=:token

-- Get a checkout key.
type GetCheckoutKeyCall =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> "checkout-key"
    :> Capture "fingerprint" Text
    :> QueryParam "circle-token" Token
    :> Get '[JSON] CheckoutKeyInfo
    -- GET: /project/:username/:project/checkout-key/:fingerprint?circle-token=:token

-- Create checkout key.
type CreateCheckoutKeyCall =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> "checkout-key"
    :> QueryParam "circle-token" Token
    :> Post '[JSON] CheckoutKeyInfo
    -- POST: /project/:username/:project/checkout-key?circle-token=:token

-- Delete a checkout key.
type DeleteCheckoutKeyCall =
       "project"
    :> Capture "username" UserName
    :> Capture "project" ProjectName
    :> "checkout-key"
    :> Capture "fingerprint" Text
    :> QueryParam "circle-token" Token
    :> Delete '[JSON] CheckoutKeyDeleted
    -- DELETE: /project/:username/:project/checkout-key/:fingerprint?circle-token=:token

-------------------------------------------------------------------------------
-- API client calls for Servant -----------------------------------------------
-------------------------------------------------------------------------------

servantGetCheckoutKeys :: UserName
                       -> ProjectName
                       -> Maybe Token
                       -> Manager
                       -> BaseUrl
                       -> ClientM [CheckoutKeyInfo]

servantGetCheckoutKey :: UserName
                      -> ProjectName
                      -> Text
                      -> Maybe Token
                      -> Manager
                      -> BaseUrl
                      -> ClientM CheckoutKeyInfo

servantCreateCheckoutKey :: UserName
                         -> ProjectName
                         -> Maybe Token
                         -> Manager
                         -> BaseUrl
                         -> ClientM CheckoutKeyInfo

servantDeleteCheckoutKey :: UserName
                         -> ProjectName
                         -> Text
                         -> Maybe Token
                         -> Manager
                         -> BaseUrl
                         -> ClientM CheckoutKeyDeleted

servantGetCheckoutKeys
 :<|> servantGetCheckoutKey
 :<|> servantCreateCheckoutKey
 :<|> servantDeleteCheckoutKey = client checkoutKeyAPI

checkoutKeyAPI :: P.Proxy CheckoutKeyAPI
checkoutKeyAPI = P.Proxy

