{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Lib.Types where

import           Data.IORef
import           Data.Map
import           Data.Text
import           Discord
import           Discord.Types

class Row a

type Token = Text
type Username = Text
type MessageText = Text
type Query = Text
type Command = Text
type MessageResult = Either RestCallErrorCode Message
data StateM = forall a. Map ChannelId (Map Int a)
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
