module Bots where

import           Data.Aeson
import           GHC.Generics
import           Network.HTTP.Simple
import qualified Data.Text as T




data TelegramBot =  TelegramBot {
              ok :: Bool
            , result :: [Results]
    } deriving (Show,Generic)

instance FromJSON TelegramBot

data Results = Results {
              updateId :: Int
            , message :: Maybe Message
            , callbackQuery :: Maybe CallbackQuery
    } deriving (Show)

instance FromJSON Results where
    parseJSON (Object v) =
            Results <$> v .: "update_id"
                    <*> v .:? "message" .!= Nothing
                    <*> v .:? "callback_query" .!= Nothing

data Message = Message {
            messageId :: Int
            , from :: From
            , chat :: Chat
            , date :: Int
            , text :: T.Text
            , replyMarkup :: Maybe InlineKeyboardMarkup
} deriving (Show)

instance FromJSON Message where
    parseJSON (Object v) = 
        Message <$> v .: "message_id"
                <*> v .: "from"
                <*> v .: "chat"
                <*> v .: "date"
                <*> v .: "text"
                <*> v .:? "reply_markup" .!= Nothing

data From = From {
              userId :: Int
            , isBot :: Bool
            , firstName :: T.Text
            , lastName :: Maybe T.Text
            , language :: Maybe T.Text
} deriving (Show)

instance FromJSON From where
    parseJSON (Object v) = 
            From <$> v .: "id"
                 <*> v .: "is_bot"
                 <*> v .: "first_name"
                 <*> v .:? "last_name" .!= Nothing
                 <*> v .:? "language_code" .!= Nothing

data Chat = Chat {
              chatId :: Int
            , cFirstName :: T.Text
            , cLastName_ :: T.Text
            , chatType :: T.Text
} deriving (Show)


instance FromJSON Chat where
    parseJSON (Object v) = 
            Chat <$> v .: "id"
                 <*> v .: "first_name"
                 <*> v .: "last_name"
                 <*> v .: "type"


data CallbackQuery = CallbackQuery {
          cBQId :: T.Text
        , cBQFrom :: From
        , cBQMessage :: Message
        , cBQinlineMessageId :: Maybe T.Text
        , cBQChatInstance :: T.Text 
        , cBQData :: T.Text
} deriving (Show)

instance FromJSON CallbackQuery where
    parseJSON (Object v) = CallbackQuery <$> v .: "id"
                                         <*> v .: "from"
                                         <*> v .: "message"
                                         <*> v .:? "inline_message_id" .!= Nothing
                                         <*> v .: "chat_instance"
                                         <*> v .: "data"

newtype InlineKeyboardMarkup = InlineKeyboardMarkup {inline_keyboard ::[[InlineKeyboardButton]]} deriving Show

instance FromJSON InlineKeyboardMarkup where
    parseJSON (Object v) = InlineKeyboardMarkup <$> v.: "inline_keyboard"

data InlineKeyboardButton = InlineKeyboardButton {
                inlineText :: T.Text
           ,    url :: Maybe T.Text 
           ,    callbackData :: Maybe T.Text 
           ,    switchInlineQuery :: Maybe T.Text 

} deriving (Show)

instance FromJSON InlineKeyboardButton where
    parseJSON (Object v) = InlineKeyboardButton
                    <$> v .: "text"
                    <*> v .:? "url" .!= Nothing
                    <*> v .:? "callback_data" .!= Nothing
                    <*> v .:? "switch_inline_query" .!= Nothing









