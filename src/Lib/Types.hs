{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Lib.Types where

import           Data.Aeson
import           Data.IORef
import           Data.Text
import           Discord
import           Discord.Types
import           GHC.Generics

data Row = Row {
    id        :: Text,
    name      :: Text,
    info_hash :: Text,
    leechers  :: Int,
    seeders   :: Int,
    num_files :: Int,
    size      :: Int,
    username  :: Text,
    added     :: Text, -- todo date
    status    :: Text,
    category  :: Int,
    imdb      :: Maybe Text
} deriving (Generic, Show)

instance FromJSON Row where
    parseJSON (Object value) = Row <$>
        value .: "id" <*>
        value .: "name" <*>
        value .: "info_hash" <*>
        (read <$> value .: "leechers") <*>
        (read <$> value .: "seeders") <*>
        (read <$> value .: "num_files") <*>
        (read <$> value .: "size") <*>
        value .: "username" <*>
        value .: "added" <*>
        value .: "status" <*>
        (read <$> value .: "category") <*>
        ((\imdbID -> if imdbID == ""
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
type StateM = [(Int, Row)]
type APIDomain = Text
type TorrentClient = Text

data Env = Env {
    envStateM        :: IORef StateM,
    envToken         :: Token,
    envCID           :: ChannelId,
    envGID           :: GuildId,
    envApiDomain     :: APIDomain,
    envTorrentClient :: TorrentClient
}
