module Methods (
    getUpdates

)

where

import qualified Token
import           Bots
import           Data.Maybe
import           Data.Aeson
import           Logger
import Data.Time
import           Network.HTTP.Simple
import           Data.Either
import qualified Data.Text                  as T 
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC




getUpdates :: Int -> IO TelegramBot
getUpdates n = do
    let req = createRequest $ "/getUpdates?offset=" <> (BC.pack . show $ n)
    getCurrentTime >>= getInfo "Get Requests"  -- Log
    fetchJSON <- httpLBS req
    let rawJSON = getResponseBody fetchJSON
    let json = decode rawJSON :: Maybe TelegramBot
    return . fromJust $ json

sendText = undefined

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



