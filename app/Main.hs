module Main where
import qualified Bot
import qualified Message
import qualified Data.Text as T
import Network.HTTP.Simple
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text.IO as TIO
import Data.Maybe
import qualified Data.Text.Encoding as E
import Control.Monad


main :: IO ()
main = do
  startBot (-1)

startBot :: Int -> IO ()
startBot n = do
  fetchJSON <- httpLBS . Message.updateRequest $ n
  let rawJSON = getResponseBody fetchJSON
  let json = decode rawJSON :: Maybe Bot.Bot
  if isNothing json
    then TIO.putStrLn "Ошибка запроса"
    else do
    print json
    let result = Bot.result . fromJust $ json
    if null result
        then startBot n
        else do
        sequence_ (requestList $ getTextId result)
        startBot $ getLastUpdateId result + 1



getLastUpdateId :: [Bot.Results] -> Int
getLastUpdateId = Bot.update_id . last 

getTextId :: [Bot.Results] -> [(Int, T.Text)]
getTextId = map helper
  where helper x = (userId x, text x)
        text x = Bot.text . Bot.message $ x
        userId x = Bot.userId . Bot.from . Bot.message $ x
  
requestList :: [(Int, T.Text)] -> [IO ()]
requestList = map (uncurry (Message.sendMessage 0))
