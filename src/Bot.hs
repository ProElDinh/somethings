module Bot where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import GHC.Generics
import Network.HTTP.Simple
import qualified Data.Text.Encoding as E
import Token
data Bot = Bot {
              ok :: Bool
            , result :: [Results]
    } deriving (Show,Generic)

data Results = Results {
              update_id :: Int
            , message :: Message
    } deriving (Show, Generic)

data Message = Message {
            message_id :: Int
            , from :: From
            , chat :: Chat
            , date :: Int
            , text :: T.Text
} deriving (Show, Generic)

data From = From {
              userId :: Int
            , isBot :: Bool
            , firstName :: T.Text
            , lastName :: T.Text
            , language :: T.Text
} deriving (Show)

data Chat = Chat {
              chatId :: Int
            , cFirstName :: T.Text
            , cLastName_ :: T.Text
            , chatType :: T.Text
} deriving (Show)

instance FromJSON Bot
instance FromJSON Results
instance FromJSON Message

instance FromJSON From where
    parseJSON (Object v) = 
            From <$> v .: "id"
                 <*> v .: "is_bot"
                 <*> v .: "first_name"
                 <*> v .: "last_name"
                 <*> v .: "language_code"

instance FromJSON Chat where
    parseJSON (Object v) = 
            Chat <$> v .: "id"
                 <*> v .: "first_name"
                 <*> v .: "last_name"
                 <*> v .: "type"


telegramHost :: BC.ByteString
telegramHost = "api.telegram.org"

getUpdates :: Int -> B.ByteString
getUpdates num = "/bot" <> botToken <> method num

method :: Int -> BC.ByteString
method num = "/getUpdates?offset=" <> (BC.pack . show $ num)

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest host method path =
    setRequestMethod method
  $ setRequestHost host
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
    defaultRequest


createRequest = buildRequest telegramHost "GET" 

updateRequest num = createRequest $ getUpdates num

createMessage :: Int -> T.Text -> Query
createMessage userId text = [("chat_id", Just . BC.pack . show $ userId),("text", Just . E.encodeUtf8 $ text) ]

sendMessage :: Int -> T.Text -> IO ()
sendMessage _ "stop" = error "Stop"
sendMessage userId text = do
          let message =   setRequestQueryString (createMessage userId text)
                        $ createRequest $ "/bot" <> botToken <> "/sendMessage"
          httpNoBody message
          return ()

