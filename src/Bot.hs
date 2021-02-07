{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.Text.IO as TIO
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Char
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

getJSON :: Int -> IO [Results]
getJSON n = do
  fetchJSON <- httpLBS $ Message.updateRequest n
  print $ getResponseStatus fetchJSON
  let rawJSON = getResponseBody fetchJSON
  print $ rawJSON
  let json = decode rawJSON :: Maybe Bot
  return . Bot.result . fromJust $ json

startBot :: Int -> State -> IO ()
startBot n cache = do
  result <- getJSON n
  if null result then 
    startBot n cache
    else do
      let last = getLastUpdateId result + 1
      let (textParametr, cache') = getInfo (getTextId result) cache
      sequence_ (requestList textParametr)
      print cache
      startBot last cache'

getTextId :: [Bot.Results] -> [(Message.UserID, Message.MessageText)]
getTextId = map helper
  where helper x = (userId x, text x)
        text x = Bot.text . Bot.message $ x
        userId x = Bot.userId . Bot.from . Bot.message $ x  

getLastUpdateId :: [Bot.Results] -> Int
getLastUpdateId = Bot.update_id . last 


requestList :: [(Int, Int, T.Text)] -> [IO ()]
requestList = map (\(n, userId, text) ->  Message.sendMessage n userId text)

data User = User {
        userID :: Int
      , activeFunction :: Bool
      , numberOfRepeat :: Int
  } deriving (Show, Eq, Ord)

newtype State = State {
    cache :: Map.Map Int User
  } deriving (Show)

defaultNumOfRepeat = 1

createUser :: Message.UserID -> User
createUser userId = User {userID = userId, activeFunction = True, numberOfRepeat = 1}

defaultUser :: User
defaultUser = User {userID = 0, activeFunction = False, numberOfRepeat = defaultNumOfRepeat}

checkInCache :: Int -> State -> Maybe User
checkInCache userId state = Map.lookup userId (cache state)

addCache :: Int -> User -> State -> State
addCache userId user state = State {cache = Map.insert userId user $ cache state}

isNum :: T.Text -> Bool
isNum text = (all isDigit . T.unpack) text && ((read . T.unpack $ text) < 7)

process :: (Int, T.Text) -> State -> ((Int, Int, T.Text), State)
process (userId, "/repeat") state = ((1, userId, "/repeat"), addCache userId (createUser userId) state)
process (userId, text) state = 
  let user = fromMaybe defaultUser (checkInCache userId state)
      newNumOfRep = read . T.unpack $ text
      text' 
        | activeFunction user && not (isNum text) = "The number of repeat entered is incorrect"
        | activeFunction user && isNum text = "The number of repeat is changed"
        | otherwise = text
      state'
        | activeFunction user && isNum text= addCache userId (user {numberOfRepeat = newNumOfRep, activeFunction = False}) state
        | otherwise = state
      info = (numberOfRepeat user ,userId, text')
  in (info, state')


getInfo :: [(Int, T.Text)] -> State -> ([(Int, Int, T.Text)], State)
getInfo [] state = ([], state)
getInfo (x:xs) state = let (zs, state) = getInfo xs state' in (info:zs, state')
  where (info, state') = process x state


run = startBot (-1) (State {cache = Map.empty})