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
              updateId      :: Int
            , message       :: Maybe Message
            , callbackQuery :: Maybe CallbackQuery
    } deriving (Show)

instance FromJSON Results where
    parseJSON (Object v) =
            Results <$> v .: "update_id"
                    <*> v .:? "message"         .!= Nothing
                    <*> v .:? "callback_query"  .!= Nothing

data Message = Message {
            messageId       :: Int
            , from          :: From
            , chat          :: Chat
            , date          :: Int
            , text          :: Maybe T.Text
            , sticker       :: Maybe Sticker
            , replyMarkup   :: Maybe InlineKeyboardMarkup
} deriving (Show)

instance FromJSON Message where
    parseJSON (Object v) = 
        Message <$> v .: "message_id"
                <*> v .: "from"
                <*> v .: "chat"
                <*> v .: "date"
                <*> v .:? "text"            .!= Nothing
                <*> v .:? "sticker"         .!= Nothing
                <*> v .:? "reply_markup"    .!= Nothing

data From = From {
              userId        :: Int
            , isBot         :: Bool
            , firstName     :: T.Text
            , lastName      :: Maybe T.Text
            , language      :: Maybe T.Text
} deriving (Show)

instance FromJSON From where
    parseJSON (Object v) = 
            From <$> v .: "id"
                 <*> v .: "is_bot"
                 <*> v .: "first_name"
                 <*> v .:? "last_name"      .!= Nothing
                 <*> v .:? "language_code"  .!= Nothing

data Chat = Chat {
              chatId        :: Int
            , cFirstName    :: T.Text
            , cLastName_    :: T.Text
            , chatType      :: T.Text
} deriving (Show)


instance FromJSON Chat where
    parseJSON (Object v) = 
            Chat <$> v .: "id"
                 <*> v .: "first_name"
                 <*> v .: "last_name"
                 <*> v .: "type"


data CallbackQuery = CallbackQuery {
          cBQId                 :: T.Text
        , cBQFrom               :: From
        , cBQMessage            :: Message
        , cBQinlineMessageId    :: Maybe T.Text
        , cBQChatInstance       :: T.Text 
        , cBQData               :: T.Text
} deriving (Show)

instance FromJSON CallbackQuery where
    parseJSON (Object v) = CallbackQuery <$> v .: "id"
                                         <*> v .: "from"
                                         <*> v .: "message"
                                         <*> v .:? "inline_message_id" .!= Nothing
                                         <*> v .: "chat_instance"
                                         <*> v .: "data"

newtype InlineKeyboardMarkup = InlineKeyboardMarkup {inline_keyboard ::[[InlineKeyboardButton]]} deriving (Show, Generic)

instance ToJSON InlineKeyboardMarkup where
    toJSON (InlineKeyboardMarkup inlinekey) = object ["inline_keyboard" .= inlinekey]


instance FromJSON InlineKeyboardMarkup where
    parseJSON (Object v) = InlineKeyboardMarkup <$> v.: "inline_keyboard"


data InlineKeyboardButton = InlineKeyboardButton {
                inlineText :: T.Text
           ,    url :: Maybe T.Text 
           ,    callbackData :: Maybe T.Text 
           ,    switchInlineQuery :: Maybe T.Text 

} deriving (Show)

instance ToJSON InlineKeyboardButton where
    toJSON (InlineKeyboardButton text url callback_data switchInlineQuery) = object [
            "text" .= text,
            "url" .= url,
            "callback_data" .= callback_data,
            "switch_inline_query" .= switchInlineQuery]

instance FromJSON InlineKeyboardButton where
    parseJSON (Object v) = InlineKeyboardButton
                    <$> v .: "text"
                    <*> v .:? "url" .!= Nothing
                    <*> v .:? "callback_data" .!= Nothing
                    <*> v .:? "switch_inline_query" .!= Nothing


data Sticker = Sticker {
            sFile_id         :: T.Text 
        ,   sFile_unique_id  :: T.Text 
        ,   sWidth           :: Int
        ,   sHeight          :: Int
        ,   sIs_animated     :: Bool
        ,   sThumb           :: PhotoSize
        ,   sEmoji           :: T.Text 
        ,   sSet_name        :: T.Text
        ,   sMask_position   :: Maybe MaskPosition
        ,   sFile_size       :: Int
} deriving (Show)

instance FromJSON Sticker where
    parseJSON (Object v) = Sticker
                    <$> v .: "file_id"
                    <*> v .: "file_unique_id" 
                    <*> v .: "width" 
                    <*> v .: "height"
                    <*> v .: "is_animated"
                    <*> v .: "thumb" 
                    <*> v .: "emoji" 
                    <*> v .: "set_name"
                    <*> v .:? "mask_position" .!= Nothing 
                    <*> v .: "file_size" 

data PhotoSize = PhotoSize {
            file_id         :: T.Text 
        ,   file_unique_id  :: T.Text 
        ,   width           :: Int
        ,   height          :: Int
        ,   file_size       :: Int
} deriving (Show, Generic)

instance FromJSON PhotoSize

data MaskPosition = MaskPosition {
            point           :: T.Text
        ,   x_shift         :: Double
        ,   y_shift         :: Double
        ,   scale           :: Double
} deriving (Show, Generic)

instance FromJSON MaskPosition

