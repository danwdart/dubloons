{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LAnGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Lib.Types where

import Data.Aeson
import           Data.IORef
import           Data.Map
import           Data.Text as T
import           Discord
import           Discord.Types
import GHC.Generics

magnetPrefix ∷ Text
magnetPrefix = "magnet:?xt=urn:btih:"

data Row = Row {
    id        :: Text,
    title     :: Text,
    infoHash :: Text,
    leechers  :: Maybe Int,
    seeders   :: Maybe Int,
    num_files :: Maybe Int,
    size      :: Maybe Int,
    username  :: Maybe Text,
    added     :: Maybe Text, -- todo date
    status    :: Maybe Text,
    category  :: Maybe Int,
    imdb      :: Maybe Text
} deriving (Generic)

-- TODO make into URI objects
trackers :: [Text]
trackers = [
    "http://anidex.moe:6969/announce",
    "http://mgtracker.org:6969/announce",
    "http://nyaa.tracker.wf:7777/announce",
    "http://sukebei.tracker.wf:7777/announce",
    "http://tracker.anirena.com:80/announce",
    "udp://9.rarbg.to:2920/announce",
    "udp://exodus.desync.com:6969/announce",
    "udp://explodie.org:6969",
    "udp://ipv6.leechers-paradise.org:6969/announce",
    "udp://open.stealth.si:80/announce",
    "udp://tracker.coppersurfer.tk:6969/announce",
    "udp://tracker.cyberia.is:6969/announce",
    "udp://tracker.internetwarriors.net:1337/announce",
    "udp://tracker.leechers-paradise.org:6969",
    "udp://tracker.leechers-paradise.org:6969/announce",
    "udp://tracker.opentrackr.org:1337/announce",
    "udp://tracker.pirateparty.gr:6969/announce",
    "udp://tracker.uw0.xyz:6969/announce",
    "udp://tracker.zer0day.to:1337/announce"
    ]

-- TODO calculate 
magnetLink :: Row -> Text
magnetLink Row { infoHash } = magnetPrefix <> infoHash <> "&tr=" <> T.intercalate "&tr=" trackers

instance Show Row where
    show Row {
        title = rowTitle,
        leechers = rowLeechers,
        seeders = rowSeeders,
        imdb = rowIMDB
    } = T.unpack rowTitle <>
        " (" <>
        show rowSeeders <>
        " seeders, " <>
        show rowLeechers <>
        " leechers)." <>
        maybe mempty (T.unpack . (" https://imdb.com/title/" <>)) rowIMDB

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
type StateM = Map ChannelId (Map Int Row)
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
