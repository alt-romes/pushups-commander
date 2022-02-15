{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module ChatBot where

import Data.Profunctor (Profunctor, dimap)
import Data.Text (Text)
import Servant (Server, serve, Proxy(..), HasServer)
import Network.Wai (Application)
import Control.Concurrent.Async (mapConcurrently_)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.Reader (ReaderT, runReaderT, MonadTrans)

-- | Run multiple chat bots, each in a specific port
--
-- @
-- slackHandler :: Server SlackEvents
-- slackHandler = ...
--
-- main = runChatBots [(slackPushupBot (slackToken, session), 25564)]
-- @
runChatBots :: HasServer a '[] => [(a #=# Bot m i o, Int)] -> IO ()
runChatBots = mapConcurrently_ (\(cb, port) -> run port (logStdoutDev $ mkApplication cb))

mkApplication :: HasServer a '[] => (a #=# Bot m i o) -> Application
mkApplication (BotServer proxy server runBot) = serve proxy (server runBot)

-- | A bot transforms an input into a contextual output
newtype Bot m i o = Bot { runBot :: i -> m o }

instance Functor f => Functor (Bot f i) where
    fmap f = Bot . fmap (fmap f) . runBot

instance Functor f => Profunctor (Bot f) where
    dimap f g = Bot . dimap f (fmap g) . runBot

-- | A ChatBot message has a server identifier, user id and textual content
class ChatBotMessage a where
    getServerId :: a -> Text
    getUserId   :: a -> Text
    getText     :: a -> Text

-- | A @Chattable m a@ means that within @m@, given a @'ChatBotMessage' a@, it is possible to react and reply to said message
class ChatBotMessage a => Chattable m a where
    reactTo :: a -> Text -> m ()
    replyTo :: a -> Text -> m ()

-- | A 'ChatBot' is a Bot that is 'Chattable', i.e., a bot that can react and reply to 'ChatBotMessage's, and whose input type is any 'ChatBotMessage'
-- type ChatBot' m i o = (Chattable m i, ChatBotMessage i) => Bot m i o

-- | To create a chat bot apply the type of the API and pass the API Server handler
--
-- @
-- slackHandler :: Token -> Server SlackEvents
-- slackHandler = ...
--
-- ... ChatBot @SlackEvents (slackHandler tok)
-- @
data a #=# b where
    BotServer :: forall a b m i o. (b ~ Bot m i o, HasServer a '[]) => Proxy a -> (Bot m i o -> Server a) -> (Bot m i o) -> (a #=# Bot m i o)

type ReaderBot r m i o = Bot (ReaderT r m) i o
