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
import            Data.Maybe
import qualified  Data.Map                as M
main :: IO ()
main = undefined 

data TextType = Message | Sticker | Videos | Picture | QueryAnswer | Repeat | Help deriving (Show)

startBot :: (MonadIO m) => StateT (M.Map Int Int) (ReaderT (Int) m) ()
startBot = do
  cache <- get
  lastId <- updateId 0
  telegramJSON <- Methods.getUpdates lastId
  when (not . null . Bots.result $ telegramJSON) (liftIO $ putStrLn "")
  liftIO $ print telegramJSON
  return ()
    where updateId num = do
            get
            return num
          func = do
              lastId <- updateId 0
              telegramJSON <- Methods.getUpdates lastId
              return ()

runBot ::(MonadIO m) => m ((), M.Map Int Int)
runBot = runReaderT (runStateT startBot M.empty) 0


processMessage :: Bots.Results -> TextType
processMessage result = case Bots.message result of
                          Nothing -> QueryAnswer
                          Just mes -> typeOfMessages mes

typeOfMessages :: Bots.Message -> TextType
typeOfMessages mes
  | isNothing $ Bots.sticker mes = Sticker
  | otherwise = typeOfText . fromJust . Bots.text $ mes

typeOfText :: T.Text ->  TextType
typeOfText "/help"    = Help
typeOfText "/repeat"  = Repeat
typeOfText _          = Message