module Message where
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import GHC.Generics
import Network.HTTP.Simple
import qualified Data.Text.Encoding as E
import Token
import Control.Monad
import qualified Data.Text.IO as TIO

type UserID = Int
type MessageText = T.Text

data Keyboard = ReplyKeyboardMarkup {
        keyboard :: [[KeyboardButton]]
      , one_time_keyboard :: Bool
      , resize_keyboard :: Bool
      , selective :: Bool
  } deriving (Show, Generic)

instance ToJSON Keyboard


newtype KeyboardButton = KeyboardButton {
      keyboardText :: T.Text
  } deriving (Show)


instance ToJSON KeyboardButton where
  toJSON (KeyboardButton text ) = object [
                 "text" .= text
    ]

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest host method path =
    setRequestMethod method
  $ setRequestHost host
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
    defaultRequest

createRequest :: BC.ByteString -> Request
createRequest method = buildRequest telegramHost "GET" ("/bot" <> botToken <> method)

telegramHost :: BC.ByteString
telegramHost = "api.telegram.org"

getUpdates :: Int -> BC.ByteString
getUpdates num = "/getUpdates?offset=" <> (BC.pack . show $ num)

updateRequest :: Int -> Request   -- getUpdates
updateRequest num = createRequest $ getUpdates num

  -- functions for messages processed
keyboardsButton :: Keyboard  -- keyboard interface for users
keyboardsButton = ReplyKeyboardMarkup { keyboard = [[KeyboardButton "1", KeyboardButton "2", KeyboardButton "3" ]
                                                   ,[KeyboardButton "4", KeyboardButton "5", KeyboardButton "6" ]]
                                                   , one_time_keyboard = True, resize_keyboard = True, selective = False}

replyMarkup keyboardsButton = [("reply_markup", Just . BC.concat . L.toChunks . encode . toJSON $ keyboardsButton)]
chatId userId = [("chat_id", Just . BC.pack . show $ userId)]
messageText text = [("text", Just "text")]


processing :: Int -> MessageText -> MessageText  -- func for processed the text
processing _ "/help" = "Здравствуйте, меня зовут Бот Дункан, чем я могу вам помочь ?"
processing n "/repeat" = "Количество текущих повторов: " <> (T.pack . show $ n) <>
                         "Поменять количество повторов на: "
processing n text = mconcat $ replicate n (text <> "\n")

defaultMessage :: Int -> UserID -> MessageText -> Request  
defaultMessage n userId text = setRequestQueryString (chatId userId <> (messageText . processing n $ text))
                             $ createRequest "/sendMessage"

messageRequest :: Int -> UserID -> MessageText -> Request  -- create request based on text
messageRequest n userId "/repeat" = setRequestQueryString (replyMarkup keyboardsButton)
                                  $ defaultMessage n userId "/repeat"
messageRequest n userId text =  defaultMessage n userId text


sendMessage :: Int -> UserID -> MessageText -> IO ()
sendMessage n userId text = do
          let message = messageRequest n userId text
          httpNoBody message
          return ()






