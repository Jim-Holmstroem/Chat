{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Applicative
import           Data.Traversable

import           GHC.Generics

import           System.Random

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           Data.UUID
import           Data.Time.Clock
import           Data.Time.ISO8601

import           Data.Aeson
import           Data.Aeson.Types

import           Database.Redis
import           Snap
import           Snap.Util.FileServe

import           Lib


-- formatISO8601


instance FromJSON UUID where
    parseJSON value@(Data.Aeson.String content) = case Data.UUID.fromText content of
        (Just uuid) -> return uuid
        Nothing -> typeMismatch "UUID" value
    parseJSON invalid = typeMismatch "UUID" invalid
instance ToJSON UUID where
    toJSON uuid = Data.Aeson.String $ Data.UUID.toText uuid


data ChatMessage = ChatMessage { uuid      :: UUID
                               , timestamp :: UTCTime
                               , user      :: BSC8.ByteString
                               , content   :: BSC8.ByteString
                               }
    deriving (Read, Show, Eq, Generic)


mkMessage :: BSC8.ByteString -> BSC8.ByteString -> IO ChatMessage
mkMessage user content = ChatMessage <$> randomIO <*> getCurrentTime <*> return user <*> return content


instance FromJSON ChatMessage
instance ToJSON ChatMessage


instance FromJSON BSC8.ByteString where
    parseJSON value@(Data.Aeson.String content) = return $ encodeUtf8 content
    parseJSON invalid = typeMismatch "ByteString" invalid
instance ToJSON BSC8.ByteString where
    toJSON = Data.Aeson.String . decodeUtf8


response :: Int -> Snap ()
response code = do
    modifyResponse $ setResponseCode code
    r <- getResponse
    finishWith r


postMessage :: Connection -> Snap ()
postMessage conn = do
    Just boardId <- getParam "boardId"
    Just user <- getPostParam "user"
    Just content <- getPostParam "content"

    message <- liftIO $ mkMessage user content
    result <- liftIO $ runRedis conn $ multiExec $ do
        lpush (BSC8.append "messages:" boardId) [Data.UUID.toASCIIBytes $ uuid message]
        set (BSC8.append "message:" $ Data.UUID.toASCIIBytes $ uuid message) $ BSLC8.toStrict $ encode message

    case result of
        (TxSuccess _) -> response 201
        TxAborted -> response 500
        TxError errorMsg -> response 500


getMessages :: Connection -> Snap ()
getMessages conn = do
    Just boardId <- getParam "boardId"
    Right messageIds <- liftIO $ runRedis conn $ lrange (BSC8.append "messages:" boardId) 0 (-1)
    messages <- liftIO $ runRedis conn $ getMessagFromIds messageIds

    mapM_ (writeBS . BSC8.pack . (++ "\n") . show) messages
        where getMessagFromIds (id:ids) = (:) <$> get (BSC8.append "message:" id)  <*> getMessagFromIds ids
              getMessagFromIds [] = pure []


--    case result of
--        (Right messages) -> return ()
--    case result of
--        (TxSuccess messageIds) -> do
--            let queries = fmap (\uuid->get $ BSC8.append "message:" uuid) messageIds
--            messages <- liftIO $ runRedis conn $ multiExec $ Data.Traversable.sequence queries
--            writeBS $ BSC8.pack $ show messages


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
