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
import Control.Concurrent.Async (mapConcurrently_)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.Reader (ReaderT, runReaderT)

runChatBots :: [(ChatBot a, Int)] -> IO ()
runChatBots = mapConcurrently_ (\(cb, port) -> run port (logStdoutDev $ mkApplication cb))

mkApplication :: forall a. ChatBot a -> Application
mkApplication (ChatBot h) = serve (Proxy @a) h

-- | To create a chat bot apply the type of the API and pass the API Server handler
--
-- @
-- slackHandler :: Token -> Server SlackEvents
-- slackHandler = ...
--
-- ... ChatBot @SlackEvents (slackHandler tok)
-- @
data ChatBot a where
    ChatBot :: HasServer a '[] => Server a -> ChatBot a

class ChatBotMessage a where
    getServerId :: a -> Text
    getUserId   :: a -> Text
    getText     :: a -> Text
    reactTo     :: a -> Text -> Chat a ()
    replyTo     :: a -> Text -> Chat a ()

type Chat a = ReaderT (ReplyMessageState a) IO
type family ReplyMessageState a

runChat :: r -> ReaderT r m a -> m a
runChat = flip runReaderT
