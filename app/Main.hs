{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.Traversable

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
import Snap.Util.FileServe

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
    ChatMessage { uuid :: UUID, timestamp :: UTCTime, user :: String, content :: String }
    deriving (Read, Show, Eq, Generic)


mkMessage :: String -> String -> IO ChatMessage
mkMessage user content = ChatMessage <$> randomIO <*> getCurrentTime <*> return user <*> return content


instance FromJSON ChatMessage
instance ToJSON ChatMessage


response :: Int -> Snap ()
response code = do
    modifyResponse $ setResponseCode code
    r <- getResponse
    finishWith r


postMessage :: Connection -> Snap ()
postMessage conn = do
    Just boardId <- getParam "boardId"
    message <- liftIO $ mkMessage "user" "content"
    result <- liftIO $ runRedis conn $ multiExec $ do
        lpush (BSC8.append "messages:" boardId) [Data.UUID.toASCIIBytes $ uuid message]
        set (BSC8.append "message:" $ Data.UUID.toASCIIBytes $ uuid message) $ BSC8.pack $ show message

    case result of
        (TxSuccess _) -> response 201
        TxAborted -> response 500
        TxError errorMsg -> response 500


getMessages :: Connection -> Snap ()
getMessages conn = do
    Just boardId <- getParam "boardId"
    result <- liftIO $ runRedis conn $ multiExec $ lrange (BSC8.append "messages:" boardId) 0 (-1)

    case result of
        (TxSuccess messageIds) -> do
            let queries = fmap (\uuid->get $ BSC8.append "message:" uuid) messageIds
            messages <- liftIO $ runRedis conn $ multiExec $ Data.Traversable.sequence queries
            writeBS $ BSC8.pack $ show messages

        TxAborted -> response 500
        TxError errorMsg -> response 500


top :: Snap ()
top = writeBS "top"


root :: Connection -> Snap ()
root conn =
    ifTop top <|>
    route [ ("messages/:boardId", method POST $ postMessage conn)
          , ("messages/:boardId", method GET $ getMessages conn)
          , ("static", serveDirectory "static")
          ]


main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    quickHttpServe $ root conn
