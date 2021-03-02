{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Lib.Types where

import           Data.Aeson
import           Data.Text     as T
import           Discord
import           Discord.Types
import           GHC.Generics
import           Text.Read

data Source = TPB | Nyaa | NyaaPantsu deriving (Show)

data Row = Row {
    id       :: Text,
    source   :: Source,
    title    :: Text,
    infoHash :: Text,
    leechers :: Maybe Int,
    seeders  :: Maybe Int,
    numFiles :: Maybe Int,
    size     :: Maybe Int,
    username :: Maybe Text,
    added    :: Maybe Text, -- todo date
    status   :: Maybe Text,
    category :: Maybe Int,
    imdb     :: Maybe Text
} deriving (Generic)

instance FromJSON Row where
    parseJSON (Object value) = Row <$>
        value .: "id" <*>
        return TPB <*>
        value .: "name" <*>
        value .: "info_hash" <*>
        (readMaybe <$> value .: "leechers") <*>
        (readMaybe <$> value .: "seeders") <*>
        (readMaybe <$> value .: "num_files") <*>
        (readMaybe <$> value .: "size") <*>
        value .:? "username" <*>
        value .:? "added" <*>
        value .:? "status" <*>
        (readMaybe <$> value .: "category") <*>
        ((\imdbID -> if T.null imdbID
            then Nothing
            else Just imdbID
        ) <$> value .: "imdb")
    parseJSON _ = error "Invalid JSON"

type Token = Text
type Username = Text
type MessageText = Text
type Query = Text
type Command = Text
type MessageResult = Either RestCallErrorCode Message
type APIDomain = Text

data Env = Env {
    envToken         :: Token,
    envCID           :: ChannelId,
    envGID           :: GuildId,
    envApiDomain     :: APIDomain
}
