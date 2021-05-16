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
import            Data.Monoid
import qualified  Data.Map                as M
main :: IO ()
main = undefined 

startBot :: (MonadIO m) => ReaderT Int (StateT (Writer (M.Map Int Int) Int) m) ()
startBot = do
  lastUpdateId <- ask
  telegramJSON <- Methods.getUpdates lastUpdateId
  liftIO $ print telegramJSON
  return ()

runBot ::(MonadIO m) => m ((), Writer (M.Map Int Int) Int)
runBot = runStateT (runReaderT (forever startBot) (0)) (writer (0,M.empty))
