module Logger where

import           Data.Time.Clock    
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Control.Monad.IO.Class
data Priority
    = Debug    -- ^ Debug messages
    | Info     -- ^ Notable information that requires no immediate action.
    | Warning  -- ^ Something is probably wrong, and we should investigate.
    | Error    -- ^ Something is wrong and immediate action is required.
    deriving (Eq, Ord, Show)

showT :: Priority -> T.Text 
showT priority = case priority of
                    Debug   -> "[Debug]   "
                    Info    -> "[Info]    "
                    Warning -> "[Warning] "
                    Error   -> "[Error]   "

newtype Handle = Handle
    { log :: Priority -> T.Text -> IO () }


showTime :: UTCTime -> T.Text
showTime time = "{" <> (T.pack . show $ time) <> "}"



getInfo ::(MonadIO m) => T.Text -> m ()
getInfo text = do
    time <- liftIO getCurrentTime
    liftIO . TIO.putStrLn . T.concat $ [showT Info, showTime time, "\t", text]