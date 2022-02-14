{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module ChatBot where

import Data.Text (Text)
import Servant (Server, serve, Proxy(..), HasServer)
import Network.Wai (Application)
import Control.Concurrent (forkIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.Reader (ReaderT)

runChatBots :: [(ChatBot a, Int)] -> IO ()
runChatBots = mapM_ (\(cb, port) -> forkIO $ run port (logStdoutDev $ mkApplication cb))

mkApplication :: forall a. ChatBot a -> Application
mkApplication (ChatBot h) = serve (Proxy @a) h

data ChatBot a where
    ChatBot :: HasServer a '[] => Server a -> ChatBot a

class ChatBotMessage a where
    getServerId    :: a -> Text
    getUserId      :: a -> Text
    createReaction :: a -> Text -> ReaderT (ReplyMessageState a) IO ()
    replyToMsg     :: a -> Text -> ReaderT (ReplyMessageState a) IO ()
    handleMsg      :: (ReplyMessageState a) -> a -> IO ()

type family ReplyMessageState a
