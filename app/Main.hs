{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class

import GHC.Generics

import System.Random

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSLC8

import Data.UUID
import Data.Time.Clock
import Data.Time.ISO8601

import Data.Aeson
import Data.Aeson.Types

import Database.Redis
import Snap

import Lib

-- formatISO8601
-- getCurrentTime


instance FromJSON UUID where
    parseJSON value@(Data.Aeson.String content) = case Data.UUID.fromText content of (Just uuid) -> return uuid
                                                                                     Nothing -> typeMismatch "UUID" value
    parseJSON invalid = typeMismatch "UUID" invalid
instance ToJSON UUID where
    toJSON uuid = Data.Aeson.String $ Data.UUID.toText uuid


data ChatMessage =
    ChatMessage { id :: UUID, timestamp :: UTCTime, content :: String }
    deriving (Read, Show, Eq, Generic)


newMessage :: String -> IO ChatMessage
newMessage content = ChatMessage <$> randomIO <*> getCurrentTime <*> return content


instance FromJSON ChatMessage
instance ToJSON ChatMessage


response500 :: Snap ()
response500 = modifyResponse $ setResponseCode 500


postMessage :: Connection -> Snap ()
postMessage conn = do
    Just boardId <- getParam "boardId"
    response <- liftIO $ runRedis conn $ lpush (BSC8.append "messages:" boardId) []

    case response of (Right _) -> modifyResponse $ setResponseCode 201
                     (Left errorMessage) -> response500


getMessages :: Connection -> Snap ()
getMessages conn = do
    Just boardId <- getParam "boardId"
    response <- liftIO $ runRedis conn $ lrange (BSC8.append "messages:" boardId) 0 (-1)

    case response of (Right messages) -> writeBS "[]"
                     (Left errorMessage) -> response500


root :: Connection -> Snap ()
root conn =
    route [ ("messages/:boardId", method POST $ postMessage conn)
          , ("messages/:boardId", method GET $ getMessages conn)
          ] 


main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    quickHttpServe $ root conn
