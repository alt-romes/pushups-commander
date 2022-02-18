{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module Bots where

import GHC.Exts (Constraint)

import Data.Functor.Identity (Identity(..))
import Data.Profunctor (Profunctor, dimap)
import Data.Text (Text)
import Servant (Server, ServerT, serve, hoistServer, Proxy(..), HasServer, Handler)
import Network.Wai (Application)
import Control.Concurrent.Async (mapConcurrently_)
import qualified Network.Wai.Handler.Warp as Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.Reader (ReaderT, runReaderT, MonadTrans)
import Control.Monad.IO.Class (MonadIO)

--- Bots ----------

-- | A bot transforms an input into a contextual output
newtype Bot m i o = Bot { runBot :: i -> m o }

instance Functor f => Functor (Bot f i) where
    fmap f = Bot . (fmap . fmap) f . runBot

instance Functor f => Profunctor (Bot f) where
    dimap f g = Bot . dimap f (fmap g) . runBot

hoistBot :: (forall o. f o -> g o) -> Bot f i o -> Bot g i o
hoistBot nt = Bot . fmap nt . runBot

transformBot :: (f o -> g o) -> Bot f i o -> Bot g i o
transformBot t = Bot . fmap t . runBot


--- Chat Bots -----

-- | A 'ChatBot' is a Bot that transforms any 'ChatBotMessage' into @['ChatBotCommand']@.
-- It can be constructed with additional constraints such as @forall m. Monad m@ and @forall m. MonadIO m@
data ChatBot p where
    ChatBot   :: (forall m i. (ChatBotConstraint p m, ChatBotMessage i) => Bot m i [ChatBotCommand]) -> ChatBot p

data AnyM
data Monadic
data MonadicIO
type family ChatBotConstraint p (m :: * -> *) :: Constraint where
    ChatBotConstraint AnyM m = ()
    ChatBotConstraint Monadic   m = Monad m
    ChatBotConstraint MonadicIO m = MonadIO m

-- | Chat Bots return a list of these. Chat Bot Servers can then use 'runChatBot' to execute them (provided they're 'Chattable')
data ChatBotCommand = ReactWith Text
                    | ReplyWith Text

-- | A ChatBot message has a server identifier, user id and textual content
class ChatBotMessage a where
    getServerId :: a -> Text
    getUserId   :: a -> Text
    getContent  :: a -> Text
    isFromBot   :: a -> Bool

-- | A @Chattable m r a@ means that given a read-only state @r@ and given a @'ChatBotMessage' a@, it is possible to react and reply to said message within monadic context @m@
class ChatBotMessage a => Chattable m a where
    reactTo :: a -> Text -> m ()
    replyTo :: a -> Text -> m ()

-- | Execute all resulting chat bot actions of a chat bot
runChatBot :: (Monad m, Chattable m i) => Bot m i [ChatBotCommand] -> i -> m ()
runChatBot bot i =
    mapM_ execute =<< runBot bot i where
        execute (ReactWith t) = reactTo i t
        execute (ReplyWith t) = replyTo i t


--- Bot Servers ---

type BotToken = Text

class Runnable a where
    run :: Int -> a -> IO ()

instance Runnable a => Runnable (Identity a) where
    run port = run port . runIdentity

instance Runnable Application where
    run port = Warp.run port . logStdoutDev

-- | A 'ChatBotServer' is just a 'Bot' that transforms a 'Bot' that receives 'ChatBotMessage's and returns @m ['ChatBotCommand']@ into a @'Runnable' a => 'Identity' a@
data ChatBotServer p where
    ChatBotServer :: (ChatBotConstraint p m, ChatBotMessage i, Runnable o') => Bot Identity (Bot m i [ChatBotCommand]) o' -> ChatBotServer p

-- | Run multiple server bots with the same behaviour bot starting on the given port and with the given states
--
-- @
-- main = runChatBots 25564 pushupsBot [(slackPushupBot, (slackToken, session))]
-- @
-- TODO: Run all applications on the same port concurrently?
runChatBotServers :: Int -> ChatBot p -> [ChatBotServer p] -> IO ()
runChatBotServers port (ChatBot bot) = mapConcurrently_ (\(ChatBotServer b) -> run port (runBot b bot))

-- runBotServersIO :: Int -> ChatBotIO -> [ChatBotServer] -> IO ()
-- runBotServersIO port bot = mapConcurrently_ (\(ChatBotServer b) -> run port (runBot b bot))
