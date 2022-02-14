module ChatBot where

import Network.Wai (Application)
import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

runChatBots :: (ChatBot c) => [(c, Int)] -> IO ()
runChatBots = mapM_ (\(cb, port) -> forkIO $ run port (logStdoutDev $ mkApplication cb))

class ChatBot c where
    mkApplication :: c -> Application
