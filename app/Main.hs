module Main where
import qualified  Bots
import qualified  Methods
import qualified  Data.Text              as T
import            Network.HTTP.Simple
import qualified  Data.ByteString        as B
import qualified  Data.ByteString.Char8  as BC
import qualified  Data.Text.IO           as TIO
import            Data.Maybe
import qualified  Data.Text.Encoding     as E
import            Control.Monad.Trans
import            Control.Monad.Writer
import            Control.Monad.Reader
import            Control.Monad.State
import            Control.Monad.Except
import            Control.Monad.IO.Class

main :: IO ()
main = undefined

startBot :: ReaderT Int IO ()
startBot = do
  lastUpdateId <- ask
  telegramJSON <- lift $ Methods.getUpdates lastUpdateId
  lift $ print telegramJSON
  return ()