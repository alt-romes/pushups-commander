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
import Servant (Server, ServerT, serve, hoistServer, Proxy(..), HasServer, Handler)
import Network.Wai (Application)
import Control.Concurrent.Async (forConcurrently_)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.Reader (ReaderT, runReaderT, MonadTrans)

-- | A bot transforms an input into a contextual output
newtype Bot m s i o = Bot { runBot :: i -> s -> m o }

instance Functor f => Functor (Bot f s i) where
    fmap f = Bot . (fmap . fmap . fmap) f . runBot

instance Functor f => Profunctor (Bot f s) where
    dimap f g = Bot . dimap f ((fmap . fmap) g) . runBot

hoistBot :: (forall o. f o -> g o) -> Bot f s i o -> Bot g s i o
hoistBot nt = Bot . (fmap . fmap) nt . runBot

transformBot :: (f o -> g o') -> Bot f s i o -> Bot g s i o'
transformBot t = Bot . (fmap . fmap) t . runBot

-- | A ChatBot message has a server identifier, user id and textual content
class ChatBotMessage a where
    getServerId :: a -> Text
    getUserId   :: a -> Text
    getText     :: a -> Text

-- | A @Chattable m r a@ means that given a reader state @r@ and given a @'ChatBotMessage' a@, it is possible to react and reply to said message within monadic context @m@
class ChatBotMessage a => Chattable m r a where
    reactTo :: r -> a -> Text -> m ()
    replyTo :: r -> a -> Text -> m ()

data ChatBotCommands = ReactWith Text
                     | ReplyWith Text
                     | Ok

runChatBotCommands :: (Monad m, Chattable m r a) => r -> a -> [ChatBotCommands] -> m ()
runChatBotCommands r a = mapM_ execute where
    execute (ReactWith t) = reactTo r a t
    execute (ReplyWith t) = replyTo r a t
    execute Ok            = pure ()

-- | A 'BotServer' is simply a bot that transforms a bot into a server application that processes requests through that bot
type BotServer m s i o   = Bot Identity s (Bot m s i o) Application
type ChatBotServer m s i = ChatBotMessage i => BotServer m s i [ChatBotCommands]
type ChatBot m s         = forall i. ChatBotMessage i => Bot m s i [ChatBotCommands]

mkBotServer :: (i -> s -> o) -> Bot Identity s i o
mkBotServer = Bot . (fmap . fmap) Identity

runBotServer :: Bot Identity s i b -> i -> s -> b
runBotServer = (fmap . fmap) runIdentity . runBot

mkBotServant :: HasServer a '[] => Proxy a -> (i -> s -> Server a) -> Bot Identity s i Application
mkBotServant proxy = mkBotServer . (fmap . fmap) (serve proxy)

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

