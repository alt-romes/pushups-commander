{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens ((%~), (.~))
import Data.Function ((&))
import Data.Text (Text, pack)
import Data.ByteString as BS (ByteString, init, readFile)

import Data.Either (fromRight)
import Network.IRC.Client as IRC
import Network.IRC.Conduit.Lens (_Privmsg)
import RecordM
import PushupsCommander

eventHandler :: RMSession -> EventHandler ()
eventHandler session = EventHandler (matchType _Privmsg) $ \source (_, Right privmsg) -> do
    runCobT session (commandHandler (getServerId source) (getUserId source) privmsg (replyTo source) (replyTo source))
         >>= either (replyTo source . pack) return

    where
        getServerId source =
            case source of
              User nick -> "IRC-DM-" <> nick
              Channel chname _ -> "IRC-" <> chname
              IRC.Server m -> error ("what to do with server message" <> show m)

        getUserId source =
            case source of
              User nick -> nick
              Channel _ nick -> nick
              IRC.Server m -> error ("what to do with server message" <> show m)

run :: RMSession -> ByteString -> Int -> Text -> IO ()
run session host port nick = do
  let conn = tlsConnection (WithDefaultConfig host port)
             -- & logfunc .~ stdoutLogger
             & onconnect .~ (defaultOnConnect >> send (Join "##pushupscommander"))
  let cfg  = defaultInstanceConfig nick & handlers %~ (eventHandler session:)
  runClient conn cfg ()

main :: IO ()
main = do
    putStrLn "Started"
    host     <- BS.init <$> BS.readFile "cob-host.secret"
    cobtoken <- BS.init <$> BS.readFile "cob-token.secret"
    session  <- makeSession host cobtoken
    run session "irc.libera.chat" 6697 "PushupsCommander"
