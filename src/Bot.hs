module Bot where
import qualified Message 
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import GHC.Generics
import Network.HTTP.Simple
import qualified Data.Text.Encoding as E
import Control.Monad.State
import qualified Data.Text.IO as TIO
import Data.Maybe
import Data.List
import qualified Data.Map as Map
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

data User = User {
        userID :: Int
      , activeFunction :: Bool
      , numberOfRepeat :: Int
  } deriving (Show, Eq, Ord)


createUser :: Message.UserID -> User
createUser userId = User {userID = userId, activeFunction = True, numberOfRepeat = defaultNumOfRepeat}

findUsersN :: Message.UserID -> Map.Map Int User -> Int
findUsersN n users = maybe defaultNumOfRepeat numberOfRepeat user
  where user = Map.lookup n users

addCache :: Int -> User -> State (Map.Map Int User) User
addCache userId user = state $ \st -> (user, Map.insert userId user st)

findInCache :: Message.UserID -> State (Map.Map Int User) Int
findInCache n = state $ \st -> (findUsersN n st, st) 

defaultNumOfRepeat :: Int
defaultNumOfRepeat = 1

getJSON :: Int -> IO [Results]
getJSON n = do
  fetchJSON <- httpLBS $ Message.updateRequest n
  print $ getResponseStatus fetchJSON
  let rawJSON = getResponseBody fetchJSON
  let json = decode rawJSON :: Maybe Bot
  return . Bot.result . fromJust $ json

startBot :: Int -> Map.Map Int User -> IO ()
startBot n cache = do
  result <- getJSON n
  let last = getLastUpdateId result
  return ()

getLastUpdateId :: [Bot.Results] -> Int
getLastUpdateId = Bot.update_id . last 

getTextId :: [Bot.Results] -> [(Message.UserID, Message.MessageText)]
getTextId = map helper
  where helper x = (userId x, text x)
        text x = Bot.text . Bot.message $ x
        userId x = Bot.userId . Bot.from . Bot.message $ x

requestList :: Int -> [(Int, T.Text)] -> [IO ()]
requestList n = map (uncurry (Message.sendMessage n))

