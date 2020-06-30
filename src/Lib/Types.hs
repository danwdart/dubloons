{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Lib.Types where

import           Data.Aeson
import           Data.IORef
import           Data.Map
import           Data.Text
import           Discord
import           Discord.Types
import           GHC.Generics

class Row

type Token = Text
type Username = Text
type MessageText = Text
type Query = Text
type Command = Text
type MessageResult = Either RestCallErrorCode Message
data StateM = Row r => Map ChannelId (Map Int r)
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
