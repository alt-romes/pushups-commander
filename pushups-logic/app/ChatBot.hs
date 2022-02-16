{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module ChatBot where

import Data.Profunctor (Profunctor, dimap)
import Data.Text (Text)
import Servant (ServerT, serve, Proxy(..), HasServer, Handler)
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
runChatBots :: HasServer a '[] => [(BotServer a s i o, Int)] -> IO ()
runChatBots = mapConcurrently_ (\(cb, port) -> run port (logStdoutDev $ mkApplication cb))

mkApplication :: HasServer a '[] => BotServer a s i o -> Application
mkApplication (BotServer proxy server bot reader) = serve proxy (server bot reader)

-- | A bot transforms an input into a contextual output
newtype Bot m s i o = Bot { runBot :: i -> s -> m o }

instance Functor f => Functor (Bot f s i) where
    fmap f = Bot . fmap (fmap (fmap f)) . runBot

instance Functor f => Profunctor (Bot f s) where
    dimap f g = Bot . dimap f (fmap (fmap g)) . runBot

-- | A ChatBot message has a server identifier, user id and textual content
class ChatBotMessage a where
    getServerId :: a -> Text
    getUserId   :: a -> Text
    getText     :: a -> Text

-- | A @Chattable m a@ means that given a reader state @r@ and given a @'ChatBotMessage' a@, it is possible to react and reply to said message within monadic context @m@
class ChatBotMessage a => Chattable m r a where
    reactTo :: r -> a -> Text -> m ()
    replyTo :: r -> a -> Text -> m ()

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
data BotServerT a (m :: * -> *) s i o where
    BotServer :: HasServer a '[] => Proxy a -> (Bot m s i o -> s -> ServerT a m) -> (Bot m s i o) -> s -> (BotServerT a m s i o)

type BotServer a s i o = BotServerT a Handler s i o

type family BotServerConstructor b where
    BotServerConstructor (BotServer a s i o) = Bot Handler s i o -> s -> BotServer a s i o

