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

data Row = TPBRow {
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
} | NyaaRow {
    title       :: Text,
    seeders     :: Maybe Int,
    leechers    :: Maybe Int,
    infoHash    :: Maybe Text
} | NyaaPantsuRow {
    title       :: Text,
    infoHash    :: Maybe Text
} deriving (Generic)

trackersCommon, trackersTPB, trackersNyaa, trackersNyaaPantsu :: [Text]
trackersCommon = [
    "udp://tracker.coppersurfer.tk:6969/announce",
    "udp://tracker.opentrackr.org:1337/announce"
    ]
trackersTPB = trackersCommon <> [
    "udp://9.rarbg.to:2920/announce",
    "udp://tracker.cyberia.is:6969/announce",
    "udp://tracker.internetwarriors.net:1337/announce",
    "udp://tracker.leechers-paradise.org:6969/announce",
    "udp://tracker.pirateparty.gr:6969/announce"
    ]
trackersNyaa = trackersCommon <> [
    "http://nyaa.tracker.wf:7777/announce",
    "udp://exodus.desync.com:6969/announce",
    "udp://open.stealth.si:80/announce"
    ]
trackersNyaaPantsu = trackersCommon <> [
    "http://anidex.moe:6969/announce",
    "http://mgtracker.org:6969/announce",
    "http://nyaa.tracker.wf:7777/announce",
    "http://sukebei.tracker.wf:7777/announce",
    "http://tracker.anirena.com:80/announce",
    "udp://explodie.org:6969",
    "udp://ipv6.leechers-paradise.org:6969/announce",
    "udp://tracker.internetwarriors.net:1337/announce",
    "udp://tracker.leechers-paradise.org:6969",
    "udp://tracker.uw0.xyz:6969/announce",
    "udp://tracker.zer0day.to:1337/announce"
    ]

magnetLink :: Row -> Text
magnetLink TPBRow { info_hash } = magnetPrefix <> info_hash <> "&tr=udp://tracker.coppersurfer.tk:6969/announce&tr=udp://9.rarbg.to:2920/announce&tr=udp://tracker.opentrackr.org:1337&tr=udp://tracker.internetwarriors.net:1337/announce&tr=udp://tracker.leechers-paradise.org:6969/announce&tr=udp://tracker.coppersurfer.tk:6969/announce&tr=udp://tracker.pirateparty.gr:6969/announce&tr=udp://tracker.cyberia.is:6969/announceinfo_hash"
magnetLink NyaaRow { infoHash } = magnetPrefix <> infohash <> "&tr=http://nyaa.tracker.wf:7777/announce&tr=udp://open.stealth.si:80/announce&tr=udp://tracker.opentrackr.org:1337/announce&tr=udp://tracker.coppersurfer.tk:6969/announce&tr=udp://exodus.desync.com:6969/announce"
magnetLink NyaaPantsuRow { infoHash } = magnetPrefix <> infoHash <> "&tr=udp://tracker.uw0.xyz:6969/announce&tr=udp://tracker.coppersurfer.tk:6969&tr=udp://tracker.zer0day.to:1337/announce&tr=udp://tracker.leechers-paradise.org:6969&tr=udp://explodie.org:6969&tr=udp://tracker.opentrackr.org:1337&tr=udp://tracker.internetwarriors.net:1337/announce&tr=http://mgtracker.org:6969/announce&tr=udp://ipv6.leechers-paradise.org:6969/announce&tr=http://nyaa.tracker.wf:7777/announce&tr=http://sukebei.tracker.wf:7777/announce&tr=http://tracker.anirena.com:80/announce&tr=http://anidex.moe:6969/announce&tr=udp://tracker.uw0.xyz:6969/announce&tr=udp://tracker.coppersurfer.tk:6969&tr=udp://tracker.zer0day.to:1337/announce&tr=udp://tracker.leechers-paradise.org:6969&tr=udp://explodie.org:6969&tr=udp://tracker.opentrackr.org:1337&tr=udp://tracker.internetwarriors.net:1337/announce&tr=http://mgtracker.org:6969/announce&tr=udp://ipv6.leechers-paradise.org:6969/announce&tr=http://nyaa.tracker.wf:7777/announce&tr=http://sukebei.tracker.wf:7777/announce&tr=http://tracker.anirena.com:80/announce&tr=http://anidex.moe:6969/announce"

instance Show Row where
    show TPBRow {
        name = rowName,
        leechers = rowLeechers,
        seeders = rowSeeders,
        imdb = rowIMDB
    } = T.unpack rowName <>
        " (" <>
        show rowSeeders <>
        " seeders, " <>
        show rowLeechers <>
        " leechers)." <>
        maybe mempty (T.unpack . (" https://imdb.com/title/" <>)) rowIMDB
    show _ = "Not yet defined."

instance FromJSON Row where
    parseJSON (Object value) = TPBRow <$>
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
