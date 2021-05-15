module Methods (
    getUpdates
    
)

where

import qualified Token
import           Bots
import           Data.Maybe
import           Data.Aeson
import           GHC.Generics
import           Logger
import Data.Time
import           Network.HTTP.Simple
import           Control.Monad
import           Data.Either
import qualified Data.Text                  as T 
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.Text.Encoding         as E
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Monad.IO.Class
type UserID = Int
type MessageText = T.Text

getUpdates :: (MonadIO m) => Int -> m TelegramBot
getUpdates n = do
    let req = createRequest $ "/getUpdates?offset=" <> (BC.pack . show $ n)
    getInfo "Get Requests"  -- Log
    fetchJSON <- httpLBS req
    let rawJSON = getResponseBody fetchJSON
    let json = decode rawJSON :: Maybe TelegramBot
    return . fromJust $ json


sendText :: (MonadIO m) => Int -> UserID -> MessageText -> m ()  -- send messages to users n-times
sendText n userId text = do
    getInfo "Отправка сообщения пользователю"
    let request = setRequestQueryString (defaultQuery n userId text) defaultMessage
    replicateM_ n (httpNoBody request)

sendInlineButton :: (MonadIO m) => Int -> UserID -> m ()  -- send inline button to change user's num of repeat
sendInlineButton n userId = do
    getInfo "Изменение количество повторов у пользователя"
    let request = setRequestQueryString (defaultQuery n userId "/repeat" <> queryReplyMarkup keyboardsButton ) defaultMessage
    httpNoBody request
    return ()


answerToQuery = undefined

sendSticker= undefined


buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest host method path =
    setRequestMethod method
  $ setRequestHost host
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
    defaultRequest

telegramHost :: BC.ByteString
telegramHost = "api.telegram.org"


createRequest :: BC.ByteString -> Request
createRequest method = buildRequest telegramHost "GET" ("/bot" <> Token.botToken <> method)

defaultMessage ::  Request  
defaultMessage = createRequest "/sendMessage"

-- keyboard interface for users
keyboardsButton :: InlineKeyboardMarkup  
keyboardsButton = InlineKeyboardMarkup { inline_keyboard = [[InlineKeyboardButton "1" (Just "") (Just "1") Nothing, InlineKeyboardButton "2" (Just "") (Just "2") Nothing, InlineKeyboardButton "3" (Just "") (Just "3") Nothing ]
                                                          , [InlineKeyboardButton "4" (Just "") (Just "4") Nothing, InlineKeyboardButton "5" (Just "") (Just "5") Nothing]]}


-- func for processed the text
processing :: Int -> MessageText -> MessageText  
processing _ "/help" = "Здравствуйте, меня зовут Бот Дункан, чем я могу вам помочь ?"
processing n "/repeat" = "Количество текущих повторов: " <> (T.pack . show $ n) <> "\n" <>
                         "Поменять количество повторов на: "
processing _ text = text <> "\n"

-- Query for sending request
queryReplyMarkup keyboardsButton = [("reply_markup", Just . BC.concat . L.toChunks . encode . toJSON $ keyboardsButton)]
queryChatId userId = [("chat_id", Just . BC.pack . show $ userId)]
queryMessageText text = [("text", Just . E.encodeUtf8  $ text)]


defaultQuery :: Int -> UserID -> MessageText -> Query
defaultQuery n userId text = queryChatId userId <> (queryMessageText . processing n $ text)

