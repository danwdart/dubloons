{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main where

import           Control.Exception
import           Control.Monad hiding (fail)
import           Control.Monad.Trans.Except
import           Data.Text                  (Text)
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
        "Failed to get the authentication token. Please set the environment variable DISCORD_AUTH_TOKEN to your token. See https://github.com/aquarial/discord-haskell/wiki/Creating-your-first-Bot for more details."
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
    ) $ const $ fail err

main ∷ IO ()
main = void $ runExceptT $ do
    io . hSetBuffering stdout $ LineBuffering
    putStrLn "Dubloons v0.6.0.0"
    putStrLn "Loading environment variables"
    [
        token,
        apiDomain
        ] <- sequence $ uncurry getDubloonsEnv <$> dubloonsEnvSettings
    putStrLn "Starting bot"
    _ <- io . runDiscord $
        runDiscordOpts Env {
            envToken = token,
            envApiDomain = apiDomain
        }
    putStrLn "Bot stopped"
