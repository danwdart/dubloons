{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults -Wno-unused-imports #-}

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.IORef
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Discord
import           Lib.Discord
import           Lib.Pirate
import           Lib.Prelude
import           Lib.Types
import           Lib.Util
import           Prelude                    hiding (print, putStrLn)
import           System.Environment
import           System.IO.Error

main ∷ IO ()
main = void $ runExceptT $ do
    putStrLn "Dubloons v0.3"
    putStrLn "Loading auth token"
    token <- catchE (ExceptT (tryJust (guard . isDoesNotExistError) (getEnv "DISCORD_AUTH_TOKEN"))) $
        const $ fail "Failed to get the authentication token. Please set the environment variable DISCORD_AUTH_TOKEN to your token & make sure you include DISCORD_CHANNEL_ID. See https://github.com/aquarial/discord-haskell/wiki/Creating-your-first-Bot for more details."
    apiDomain <- catchE (ExceptT (tryJust (guard . isDoesNotExistError) (getEnv "API_DOMAIN"))) $
        const $ fail "Failed to get the API domain. Please set the environment variable API_DOMAIN."
    torrentClient <- catchE (ExceptT (tryJust (guard . isDoesNotExistError) (getEnv "TORRENT_CLIENT"))) $
        const $ fail "Failed to get the torrent client. Please set the environment variable TORRENT_CLIENT."
    cid <- catchE (ExceptT (tryJust (guard . isDoesNotExistError) (getEnv "DISCORD_CHANNEL_ID"))) $
        const $ fail "Failed to get the channel ID. Please set the environment variable DISCORD_CHANNEL_ID."
    gid <- catchE (ExceptT (tryJust (guard . isDoesNotExistError) (getEnv "DISCORD_GUILD_ID"))) $
        const $ fail "Failed to get the guild ID. Please set the environment variable DISCORD_GUILD_ID."
    putStrLn "Starting bot"
    stateM <- liftIO . newIORef $ []
    _ <- liftIO . runDiscord $
        runDiscordOpts Env {
            envStateM = stateM,
            envToken = T.pack token,
            envCID = fromIntegral $ read cid,
            envGID = fromIntegral $ read gid,
            envApiDomain = T.pack apiDomain,
            envTorrentClient = T.pack torrentClient
        }
    putStrLn "Bot stopped"
