{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Control.Exception
import           Control.Monad                  hiding (fail)
import           Control.Monad.Trans.Except
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Discord
import           Discord.Internal.Types.Prelude
import           Lib.Discord
import           Lib.Prelude
import           Lib.Types
import           Prelude                        hiding (fail, print, putStrLn)
import           System.IO                      hiding (putStrLn)
import           System.IO.Error

type EnvVariable = Text
type EnvError = Text
type EnvReturn = Text
type EnvSettings = [(EnvVariable, EnvError)]

dubloonsEnvSettings ∷ EnvSettings
dubloonsEnvSettings = [
    (
        "DISCORD_AUTH_TOKEN",
        "Failed to get the authentication token. Please set the environment variable DISCORD_AUTH_TOKEN to your token. See https://discord.com/developers/applications for more details."
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
    )
    ]

getDubloonsEnv ∷ EnvVariable → EnvError → ExceptT IOException IO EnvReturn
getDubloonsEnv var err = catchE (
        ExceptT (
            tryJust (guard . isDoesNotExistError) (getEnv var)
        )
    ) . const $ fail err

main ∷ IO ()
main = void . runExceptT $ (do
    io . hSetBuffering stdout $ LineBuffering
    putStrLn ("Dubloons v0.5.2.0" :: Text)
    putStrLn ("Loading environment variables" :: Text)
    [
        token,
        cid,
        gid,
        apiDomain
        ] <- sequence $ uncurry getDubloonsEnv <$> dubloonsEnvSettings
    putStrLn ("Starting bot" :: Text)
    _ <- io . runDiscord $
        runDiscordOpts Env {
            envToken = token,
            envCID = fromIntegral (read (T.unpack cid) :: Integer) :: Snowflake,
            envGID = fromIntegral (read (T.unpack gid) :: Integer) :: Snowflake,
            envApiDomain = apiDomain
        }
    putStrLn ("Bot stopped" :: Text))
