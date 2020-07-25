{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Main where

import           Control.Exception
import           Control.Monad hiding (fail)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Discord
import           Lib.Discord
import           Lib.Prelude
import           Lib.Types
import           Prelude                    hiding (fail, print, putStrLn)
import           System.IO hiding (putStrLn)
import           System.IO.Error

type EnvVariable = Text
type EnvError = Text
type EnvReturn = Text
type EnvSettings = [(EnvVariable, EnvError)]

dubloonsEnvSettings ∷ EnvSettings
dubloonsEnvSettings = [
    (
        "DISCORD_AUTH_TOKEN",
        "Failed to get the authentication token. Please set the environment variable DISCORD_AUTH_TOKEN to your token & make sure you include DISCORD_CHANNEL_ID. See https://github.com/aquarial/discord-haskell/wiki/Creating-your-first-Bot for more details."
    ),
    (
        "DISCORD_CHANNEL_ID",
        "Failed to get the channel ID. Please set the environment variable DISCORD_CHANNEL_ID."
    ),
    (
        "DISCORD_GUILD_ID",
        "Failed to get the guild ID. Please set the environment variable DISCORD_GUILD_ID."
    ),
    (
        "API_DOMAIN",
        "Failed to get the API domain. Please set the environment variable API_DOMAIN."
    ),
    (
        "TORRENT_CLIENT",
        "Failed to get the torrent client. Please set the environment variable TORRENT_CLIENT."
    )
    ]

getDubloonsEnv ∷ EnvVariable → EnvError → ExceptT IOException IO EnvReturn
getDubloonsEnv var err = catchE (
        ExceptT (
            tryJust (guard . isDoesNotExistError) (getEnv var)
        )
    ) $ const $ fail err

main ∷ IO ()
main = void $ runExceptT $ do
    liftIO . hSetBuffering stdout $ LineBuffering
    putStrLn "Dubloons v0.5"
    putStrLn "Loading environment variables"
    {-
        env <- sequence $ uncurry getDubloonsEnv <$> dubloonsEnvSettings
        foldl (\a b -> a <*> b) (Env <$> liftIO (newIORef []))
    -}
    [
        token,
        cid,
        gid,
        apiDomain,
        torrentClient
        ] <- sequence $ uncurry getDubloonsEnv <$> dubloonsEnvSettings
    putStrLn "Starting bot"
    _ <- io . runDiscord $
        runDiscordOpts Env {
            envToken = token,
            envCID = fromIntegral $ read (T.unpack cid),
            envGID = fromIntegral $ read (T.unpack gid),
            envApiDomain = apiDomain,
            envTorrentClient = torrentClient
        }
    putStrLn "Bot stopped"
