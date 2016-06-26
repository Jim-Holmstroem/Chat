{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Applicative
import           Data.Traversable
import           Data.Maybe
import           Data.Either

import           GHC.Generics

import           System.Random

import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSLC8

import qualified Data.Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           Data.UUID
import           Data.Time.Clock
import           Data.Time.ISO8601

import           Data.Aeson as JSON
import           Data.Aeson.Types

import           Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Text

import           Database.Redis hiding (decode)

import           Snap
import           Snap.Util.FileServe

import           Lib


instance FromJSON UUID where
    parseJSON value@(JSON.String content) = case Data.UUID.fromText content of
        (Just uuid) -> return uuid
        Nothing -> typeMismatch "UUID" value
    parseJSON invalid = typeMismatch "UUID" invalid
instance ToJSON UUID where
    toJSON uuid = JSON.String $ Data.UUID.toText uuid


instance FromJSON BSC8.ByteString where
    parseJSON value@(JSON.String content) = return $ encodeUtf8 content
    parseJSON invalid = typeMismatch "ByteString" invalid
instance ToJSON BSC8.ByteString where
    toJSON = JSON.String . decodeUtf8


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


f :: Connection -> BSC8.ByteString -> IO [ChatMessage]
f conn boardId = do
    Right messageIds <- liftIO $ runRedis conn $ lrange (BSC8.append "messages:" boardId) 0 (-1)
    readMessages <- liftIO $ runRedis conn $ getMessagesFromIds messageIds
    return $ catMaybes $ map decodeStrict $ catMaybes $ rights readMessages
        where getMessagesFromIds (id:ids) = (:) <$> get (BSC8.append "message:" id) <*> getMessagesFromIds ids
              getMessagesFromIds [] = pure []


getMessages :: Connection -> Snap ()
getMessages conn = do
    Just boardId <- getParam "boardId"

    messages <- liftIO $ f conn boardId

    writeLBS $ encode $ messages


instance ToMarkup ChatMessage where
    toMarkup (ChatMessage uuid timestamp user content) = do
        H.b $ H.toHtml $ decodeUtf8 user `Data.Text.append` ": "
        H.toHtml $ decodeUtf8 content


board :: Connection -> Snap ()
board conn = do
    Just boardId <- getParam "boardId"

    messages <- liftIO $ f conn  boardId

    writeLazyText $ renderHtml $ H.html $ do
        H.head $ do
            H.meta H.! A.httpEquiv "cache-control" H.! A.content "max-age=0"
            H.meta H.! A.httpEquiv "cache-control" H.! A.content "no-cache"
            H.meta H.! A.httpEquiv "expires" H.! A.content "0"
            H.meta H.! A.httpEquiv "expires" H.! A.content "Tue, 01 Jan 1980 1:00:00 GMT"
            H.meta H.! A.httpEquiv "pragma" H.! A.content "no-cache"
        H.body $ do
            H.form H.! A.action (H.stringValue $ "/messages/" ++ BSC8.unpack boardId) H.! A.method "post" $ do
               H.toHtml ("User:" :: Data.Text.Text)
               H.input H.! A.type_ "text" H.! A.name "user"
               H.toHtml ("Content:" :: Data.Text.Text)
               H.input H.! A.type_ "text" H.! A.name "content"
               H.input H.! A.type_ "submit" H.! A.value "Submit"

            H.ul $ mapM_ (H.li . toMarkup) messages


top :: Snap ()
top = writeBS "top"


root :: Connection -> Snap ()
root conn =
    ifTop top <|>
    route [ ("messages/:boardId", method POST $ postMessage conn)
          , ("messages/:boardId", method GET $ getMessages conn)
          , ("boards/:boardId", method GET $ board conn)
          , ("static", serveDirectory "static")
          ]


main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    quickHttpServe $ root conn
