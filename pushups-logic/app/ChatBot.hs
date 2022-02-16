{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module ChatBot where

import Data.Functor.Identity (Identity(..))
import Data.Profunctor (Profunctor, dimap)
import Data.Text (Text)
import Servant (Server, serve, Proxy(..), HasServer, Handler)
import Network.Wai (Application)
import Control.Concurrent.Async (forConcurrently_)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.Reader (ReaderT, runReaderT, MonadTrans)

-- | Run multiple server bots with the same behaviour bot starting on the given port and with the given states
--
-- @
-- main = runChatBots 25564 pushupsBot [(slackPushupBot, (slackToken, session))]
-- @
-- TODO: Run all applications on the same port concurrently?
runBotServers :: Int -> Bot m s i o -> [BotServer m s i o] -> [s] -> IO ()
runBotServers port_ bot servers states =
    let ports = [port_..port_+length servers-1] in
    forConcurrently_ (zip3 servers states ports) $ \(botserver, state, port) ->
        run port (logStdoutDev (runBotServer botserver bot state))

-- | A bot transforms an input into a contextual output
newtype Bot m s i o = Bot { runBot :: i -> s -> m o }

instance Functor f => Functor (Bot f s i) where
    fmap f = Bot . (fmap . fmap . fmap) f . runBot

instance Functor f => Profunctor (Bot f s) where
    dimap f g = Bot . dimap f ((fmap . fmap) g) . runBot

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
-- data BotServerT a (m :: * -> *) s i o where
--     BotServer :: HasServer a '[] => Proxy a -> (Bot m s i o -> s -> ServerT a m) -> (Bot m s i o) -> s -> (BotServerT a m s i o)

-- | A 'BotServer' is simply a bot that transforms a bot into a server application that processes requests through that bot
type BotServer m s i o = Bot Identity s (Bot m s i o) Application

mkBotServer :: (i -> s -> o) -> Bot Identity s i o
mkBotServer = Bot . (fmap . fmap) Identity

runBotServer :: Bot Identity s i b -> i -> s -> b
runBotServer = (fmap . fmap) runIdentity . runBot

mkBotServant :: HasServer a '[] => Proxy a -> (i -> s -> Server a) -> Bot Identity s i Application
mkBotServant proxy = mkBotServer . (fmap . fmap) (serve proxy)

